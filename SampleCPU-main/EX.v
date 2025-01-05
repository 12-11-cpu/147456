`include "lib/defines.vh"
module EX(
    input wire clk,
    input wire rst,
    // input wire flush,
    input wire [`StallBus-1:0] stall,

    input wire [`ID_TO_EX_WD-1:0] id_to_ex_bus,

    output wire [`EX_TO_MEM_WD-1:0] ex_to_mem_bus,
    
    output wire [37:0] ex_to_id_bus,//EX段数据递回ID段

    output wire data_sram_en,
    output wire [3:0] data_sram_wen,
    output wire [31:0] data_sram_addr,
    output wire [31:0] data_sram_wdata,
    output wire inst_is_load,//宽度1位，用于流水线控制，避免加载指令导致的数据冲突。
    
    //
    output wire stallreq_for_ex,//执行ex段的停顿请求信号
    output wire [65:0]ex_to_mem1,//高 32 位和低 32 位需要分别写入 hi 和 lo 寄存器
    output wire [65:0]ex_to_id_2,// hi 和 lo 寄存器中的值写回ID段
    output wire ready_ex_to_id
);

    reg [`ID_TO_EX_WD-1:0] id_to_ex_bus_r;

    always @ (posedge clk) begin
        if (rst) begin
            id_to_ex_bus_r <= `ID_TO_EX_WD'b0;
        end
       
        else if (stall[2]==`Stop && stall[3]==`NoStop) begin
            id_to_ex_bus_r <= `ID_TO_EX_WD'b0;
        end
        else if (stall[2]==`NoStop) begin
            id_to_ex_bus_r <= id_to_ex_bus;
        end
    end

    //信号
    wire [31:0] ex_pc, inst;
    wire [11:0] alu_op;
    wire [2:0] sel_alu_src1;
    wire [3:0] sel_alu_src2;
    wire data_ram_en;//支持多种数据读取模式。
    wire [3:0] data_ram_wen;
    wire [3:0] data_ram_read;
    wire rf_we;
    wire [4:0] rf_waddr;
    wire sel_rf_res;
    wire [31:0] rf_rdata1, rf_rdata2;
    reg is_in_delayslot;
    wire [1:0] lo_hi_r;//用于控制 hi 和 lo 寄存器的读取操作
    wire [1:0] lo_hi_w;//用于控制 hi 和 lo 寄存器的写入操作。
    wire w_hi_we;//控制是否将数据写入 hi 寄存器
    wire w_lo_we;//控制是否将数据写入 lo 寄存器
    wire w_hi_we3;//控制是否将数据写入 hi 寄存器（直接操作指令）
    wire w_lo_we3;//控制是否将数据写入 lo 寄存器（直接操作指令）
    wire [31:0] hi_i;//存储要写入 hi 寄存器的数据
    wire [31:0] lo_i;//存储要写入 lo 寄存器的数据
    wire[31:0] hi_o;//存储从 hi 寄存器读取的数据
    wire[31:0] lo_o;//存储从 lo 寄存器读取的数据

//增加总线宽度，添加信号
    assign {
        ex_pc,          // 158:127
        inst,           // 126:95
        alu_op,         // 94:83
        sel_alu_src1,   // 82:80
        sel_alu_src2,   // 79:76
        data_ram_en,    // 75
        data_ram_wen,   // 74:71
        rf_we,          // 70            // write
        rf_waddr,       // 69:65         //write
        sel_rf_res,     // 64
        rf_rdata1,         // 63:32      //rs
        rf_rdata2,          // 31:0      //rt
        lo_hi_r,                        //read ź 
        lo_hi_w,                        //write ź 
        lo_o,                           //loֵ
        hi_o,                            //hiֵ
        data_ram_read
    } = id_to_ex_bus_r;
    
    
    
    //控制 hi 和 lo 寄存器的写入操作
    //识别加载指令避免数据相关
    assign w_lo_we3 = lo_hi_w[0]==1'b1 ? 1'b1:1'b0;
    assign w_hi_we3 = lo_hi_w[1]==1'b1 ? 1'b1:1'b0;
    
    assign inst_is_load =  (inst[31:26] == 6'b10_0011) ? 1'b1 :1'b0;
    
    //立即数扩展
    wire [31:0] imm_sign_extend, imm_zero_extend, sa_zero_extend;
    assign imm_sign_extend = {{16{inst[15]}},inst[15:0]};
    assign imm_zero_extend = {16'b0, inst[15:0]};
    assign sa_zero_extend = {27'b0,inst[10:6]};

    wire [31:0] alu_src1, alu_src2;
    wire [31:0] alu_result, ex_result;
//ALU 输入选择
    assign alu_src1 = sel_alu_src1[1] ? ex_pc :
                      sel_alu_src1[2] ? sa_zero_extend : rf_rdata1;

    assign alu_src2 = sel_alu_src2[1] ? imm_sign_extend :
                      sel_alu_src2[2] ? 32'd8 :
                      sel_alu_src2[3] ? imm_zero_extend : rf_rdata2;
    //ALU实例化
    alu u_alu(
    	.alu_control (alu_op ),
        .alu_src1    (alu_src1    ),
        .alu_src2    (alu_src2    ),
        .alu_result  (alu_result  )
    );
//选择执行阶段（EX）的结果
    assign ex_result =  lo_hi_r[0] ? lo_o :
                         lo_hi_r[1] ? hi_o :
                         alu_result;

//控制数据存储器的访问：
    assign data_sram_en = data_ram_en ;
    assign data_sram_wen = (data_ram_read==4'b0101 && ex_result[1:0] == 2'b00 )? 4'b0001: 
                            (data_ram_read==4'b0101 && ex_result[1:0] == 2'b01 )? 4'b0010:
                            (data_ram_read==4'b0101 && ex_result[1:0] == 2'b10 )? 4'b0100:
                            (data_ram_read==4'b0101 && ex_result[1:0] == 2'b11 )? 4'b1000:
                            (data_ram_read==4'b0111 && ex_result[1:0] == 2'b00 )? 4'b0011:
                            (data_ram_read==4'b0111 && ex_result[1:0] == 2'b10 )? 4'b1100:
                            data_ram_wen;
    //生成存储器地址：
    assign data_sram_addr = ex_result ;
    //生成写入数据：
    assign data_sram_wdata = data_sram_wen==4'b1111 ? rf_rdata2 : 
                              data_sram_wen==4'b0001 ? {24'b0,rf_rdata2[7:0]} :
                              data_sram_wen==4'b0010 ? {16'b0,rf_rdata2[7:0],8'b0} :
                              data_sram_wen==4'b0100 ? {8'b0,rf_rdata2[7:0],16'b0} :
                              data_sram_wen==4'b1000 ? {rf_rdata2[7:0],24'b0} :
                              data_sram_wen==4'b0011 ? {16'b0,rf_rdata2[15:0]} :
                              data_sram_wen==4'b1100 ? {rf_rdata2[15:0],16'b0} :
                              32'b0;
   
    assign ex_to_mem_bus = {
        ex_pc,          // 75:44
        data_ram_en,    // 43
        data_ram_wen,   // 42:39
        sel_rf_res,     // 38
        rf_we,          // 37
        rf_waddr,       // 36:32
        ex_result,       // 31:0
        data_ram_read
    };
   
//将执行阶段（EX阶段）的部分结果传递到指令译码阶段（ID阶段）
    assign ex_to_id_bus = {
        rf_we,          // 37
        rf_waddr,       // 36:32
        ex_result       // 31:0
    };
    //MLU part
    wire w_hi_we1;
    wire w_lo_we1;
    wire mult;//检测有符号乘法指令
    wire multu;//检测无符号乘法指令
    //寄存器类型指令
    assign mult = (inst[31:26] == 6'b00_0000) & (inst[15:6] == 10'b0000000000) & (inst[5:0] == 6'b01_1000);
    assign multu= (inst[31:26] == 6'b00_0000) & (inst[15:6] == 10'b0000000000) & (inst[5:0] == 6'b01_1001);
    //是否需要将乘法结果写入 HI 和 LO 寄存器。
    assign w_hi_we1 = mult | multu ;
    assign w_lo_we1 = mult | multu ;
    
 //乘法结果需要写入 HI 和 LO 寄存器
    wire [63:0] mul_result;
    wire mul_ready_i;
    wire mul_begin;
    wire mul_signed;
    assign mul_signed = mult;
    assign mul_begin = mult | multu ;
//实例化乘法模块
    mul_plus u_mul_plus(
    	.clk        (clk            ),
    	.start_i      (mul_begin),
    	.mul_sign     (mul_signed),
        .opdata1_i    (  rf_rdata1    ), 
        .opdata2_i    (  rf_rdata2    ), 
        .result_o     (mul_result     ), 
        .ready_o      (mul_ready_i      )
    );

    // DIV part
    wire [63:0] div_result;
    wire inst_div, inst_divu;
    wire div_ready_i;
    reg stallreq_for_div;
    wire w_hi_we2;
    wire w_lo_we2;
   //请求流水线停顿（stall）
    assign stallreq_for_ex = (stallreq_for_div & div_ready_i==1'b0) | (mul_begin & mul_ready_i==1'b0);
   //用于指示除法或乘法操作是否完成。
    assign ready_ex_to_id = div_ready_i | mul_ready_i;
    
    assign inst_div = (inst[31:26] == 6'b00_0000) & (inst[15:6] == 10'b0000000000) & (inst[5:0] == 6'b01_1010);
    assign inst_divu= (inst[31:26] == 6'b00_0000) & (inst[15:6] == 10'b0000000000) & (inst[5:0] == 6'b01_1011);
    //用于控制是否将除法结果写入 HI 和 LO 寄存器。
    assign w_hi_we2 = inst_div | inst_divu;
    assign w_lo_we2 = inst_div | inst_divu;
    

    reg [31:0] div_opdata1_o;
    reg [31:0] div_opdata2_o;
    reg div_start_o;
    reg signed_div_o;
    


    div u_div(
    	.rst          (rst          ),
        .clk          (clk          ),
        .signed_div_i (signed_div_o ),
        .opdata1_i    (div_opdata1_o    ),
        .opdata2_i    (div_opdata2_o    ),
        .start_i      (div_start_o      ),
        .annul_i      (1'b0      ),
        .result_o     (div_result     ), //        ? 64bit
        .ready_o      (div_ready_i      )
    );
//根据除法单元的状态（div_ready_i），控制除法操作的启动、停止以及流水线的停顿。
    always @ (*) begin
        if (rst) begin
            stallreq_for_div = `NoStop;
            div_opdata1_o = `ZeroWord;
            div_opdata2_o = `ZeroWord;
            div_start_o = `DivStop;
            signed_div_o = 1'b0;
        end
        else begin
            stallreq_for_div = `NoStop;
            div_opdata1_o = `ZeroWord;
            div_opdata2_o = `ZeroWord;
            div_start_o = `DivStop;
            signed_div_o = 1'b0;
            case ({inst_div,inst_divu})
                2'b10:begin
                    if (div_ready_i == `DivResultNotReady) begin
                        div_opdata1_o = rf_rdata1;
                        div_opdata2_o = rf_rdata2;
                        div_start_o = `DivStart;
                        signed_div_o = 1'b1;
                        stallreq_for_div = `Stop;
                    end
                    else if (div_ready_i == `DivResultReady) begin
                        div_opdata1_o = rf_rdata1;
                        div_opdata2_o = rf_rdata2;
                        div_start_o = `DivStop;
                        signed_div_o = 1'b1;
                        stallreq_for_div = `NoStop;
                    end
                    else begin
                        div_opdata1_o = `ZeroWord;
                        div_opdata2_o = `ZeroWord;
                        div_start_o = `DivStop;
                        signed_div_o = 1'b0;
                        stallreq_for_div = `NoStop;
                    end
                end
                2'b01:begin
                    if (div_ready_i == `DivResultNotReady) begin
                        div_opdata1_o = rf_rdata1;
                        div_opdata2_o = rf_rdata2;
                        div_start_o = `DivStart;
                        signed_div_o = 1'b0;
                        stallreq_for_div = `Stop;
                    end
                    else if (div_ready_i == `DivResultReady) begin
                        div_opdata1_o = rf_rdata1;
                        div_opdata2_o = rf_rdata2;
                        div_start_o = `DivStop;
                        signed_div_o = 1'b0;
                        stallreq_for_div = `NoStop;
                    end
                    else begin
                        div_opdata1_o = `ZeroWord;
                        div_opdata2_o = `ZeroWord;
                        div_start_o = `DivStop;
                        signed_div_o = 1'b0;
                        stallreq_for_div = `NoStop;
                    end
                end
                default:begin
                end
            endcase
        end
    end
    
    // 选择 LO 寄存器的写数据源
assign lo_i = w_lo_we1 ? mul_result[31:0] :  // 如果 w_lo_we1 为 1，选择乘法结果的低 32 位
               w_lo_we2 ? div_result[31:0] : // 如果 w_lo_we2 为 1，选择除法结果的低 32 位
               w_lo_we3 ? rf_rdata1 :        // 如果 w_lo_we3 为 1，选择寄存器文件的数据
               32'b0;                        // 默认值为 0

// 选择 HI 寄存器的写数据源
assign hi_i = w_hi_we1 ? mul_result[63:32] : // 如果 w_hi_we1 为 1，选择乘法结果的高 32 位
               w_hi_we2 ? div_result[63:32] : // 如果 w_hi_we2 为 1，选择除法结果的高 32 位
               w_hi_we3 ? rf_rdata1 :         // 如果 w_hi_we3 为 1，选择寄存器文件的数据
               32'b0;                         // 默认值为 0

// 合并 HI 寄存器的写使能信号
assign w_hi_we = w_hi_we1 | w_hi_we2 | w_hi_we3; // 如果任何一个写使能信号为 1，则 w_hi_we 为 1

// 合并 LO 寄存器的写使能信号
assign w_lo_we = w_lo_we1 | w_lo_we2 | w_lo_we3; // 如果任何一个写使能信号为 1，则 w_lo_we 为 1

// 将 HI 和 LO 寄存器的写使能信号和写数据打包到 ex_to_mem1 总线中
assign ex_to_mem1 =
{
    w_hi_we,  // HI 寄存器的写使能信号
    w_lo_we,  // LO 寄存器的写使能信号
    hi_i,     // HI 寄存器的写数据
    lo_i      // LO 寄存器的写数据
};

// 将 HI 和 LO 寄存器的写使能信号和写数据打包到 ex_to_id_2 总线中
assign ex_to_id_2 =
{
    w_hi_we,  // HI 寄存器的写使能信号
    w_lo_we,  // LO 寄存器的写使能信号
    hi_i,     // HI 寄存器的写数据
    lo_i      // LO 寄存器的写数据
};
    
    
endmodule