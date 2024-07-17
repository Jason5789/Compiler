/* Definition section */
%{
    #include "compiler_util.h"
    #include "main.h"
    #include "stdlib.h"
    #include "string.h"

    typedef struct SymbolTColumn{
        int index;
        char* name;
        int type;
        int addr;
        int lineno;
        char* func_sig;
        int scope_level;
        int print;
        struct SymbolTColumn* next;
    }SymbolTColumn;

    typedef struct SymbolT{
        SymbolTColumn* head;
    }SymbolT;

    int yydebug = 1;
    int address = 0;
    int scope = -1;
    int type_no = 13;
    int change_line = 0;
    int cur_value_type = 1;
    int store_addr = 0;
    int cur_addr = 0;
    int lineType = 0;
    int loopNum = 0;
    int stack_type[30];
    int stack_if[30];
    int stack_if_counter = 0;
    int while_count = 0;
    int stack_for[30];
    int stack_for_counter = 0;
    int array_counter = 0;
    int array_mode = 0;
    int array_dim = 0;
    int func_if = 0;
    int func_sig_array[30];
    int func_sig_counter = 0;
    int tempFuncSig_counter = 0;
    char tempFuncSig[10][80];
    SymbolT* syT = NULL;
    SymbolTColumn* tailColumn = NULL;
    FILE *jasminFile;

    void init_jasminFile();
    void writeFunc_jasminFile();
    void leaveFunc_jasminFile(int type,char*id);
    SymbolT* crt_SymbolT(int insertScope);
    void ins_SymbolT(int type,char* name,int addr,int lineno,char* func_sig,int currentScope);
    char* find_call(char* id);
    char* change_num_to_type(int currentType);
    int find_index(int currentScope);
    int find_func_type(char* sig);
    int find_ident_address(char* id);
    int find_ident_type(char* id);
    int find_ident_type_inAddr(int address);
    void ins_func_sig(char *id);
    void construct_func(char *id);
    void fprint_funcsig();
    void fprintf_type();
    void token_GTRGEQLEQLES(int action);
    void token_EQLNEQNOT(int action);
    void token_INCDEC(int action);
    void token_IFELSE(int action);
    void token_FOR(int action);
    void token_BNT();
    void transformIdentType(int type);
%}

/* Variable or self-defined structure */
%union {
    ObjectType var_type;

    bool b_var;
    char c_var;
    int32_t i_var;
    int64_t l_var;
    float f_var;
    double d_var;
    char *s_var;

    Object obj_val;
}

/* Token without return */
%token COUT
%token SHR SHL BAN BOR BNT BXO ADD SUB MUL DIV REM NOT GTR LES GEQ LEQ EQL NEQ LAN LOR
%token VAL_ASSIGN ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN DIV_ASSIGN REM_ASSIGN BAN_ASSIGN BOR_ASSIGN BXO_ASSIGN SHR_ASSIGN SHL_ASSIGN INC_ASSIGN DEC_ASSIGN
%token IF ELSE FOR WHILE RETURN BREAK CONTINUE

/* Token with return, which need to sepcify type */
%token <var_type> VARIABLE_T
%token <b_var> BOOL_LIT
%token <c_var> CHAR_LIT
%token <i_var> INT_LIT
%token <f_var> FLOAT_LIT
%token <s_var> STR_LIT
%token <s_var> IDENT

/* Nonterminal with return, which need to sepcify type */
%type <obj_val> Expr
%type <obj_val> ExprRightSide
%type <obj_val> ExprDoubleAnd
%type <obj_val> ExprOr
%type <obj_val> ExprXor
%type <obj_val> ExprAnd
%type <obj_val> ExprSame
%type <obj_val> ExprBigOrSmall
%type <obj_val> ExprBitwise
%type <obj_val> ExprPlus
%type <obj_val> ExprCross
%type <obj_val> ExprFrontUnary
%type <obj_val> ExprPostfix
%type <obj_val> ExprValue
%type <obj_val> ExprLeftSide

%left ADD SUB
%left MUL DIV REM

%nonassoc THEN
%nonassoc ELSE

/* Yacc will start at this nonterminal */
%start Program

%%
/* Grammar section */

Program
    : { init_jasminFile(); syT=crt_SymbolT(++scope); } GlobalStmtList { scope--; fclose(jasminFile); }
    |
;

GlobalStmtList 
    : GlobalStmtList GlobalStmt
    | GlobalStmt
;

GlobalStmt
    : DefineVariableStmt
    | FunctionDefStmt
;

DefineVariableStmt
    : VARIABLE_T IDENT VAL_ASSIGN ExprRightSide ';'
;

/* Function */
FunctionDefStmt
    : VARIABLE_T IDENT { address=0; if(strcmp($<s_var>2,"mod") == 0){func_if=1;} ins_SymbolT(0,$<s_var>2,-1,yylineno,"unknown",scope); } '(' { syT=crt_SymbolT(++scope); } FunctionParameterStmtList ')' { func_sig_array[func_sig_counter++]=$<var_type>1; construct_func($<s_var>2); ins_func_sig($<s_var>2); writeFunc_jasminFile($<s_var>2,find_call($<s_var>2)); } '{' StmtList '}' {leaveFunc_jasminFile($<var_type>1,$<s_var>2); scope--;}
;

FunctionParameterStmtList
    : FunctionParameterStmtList ',' FunctionParameterStmt
    | FunctionParameterStmt
    |
;

FunctionParameterStmt
    : VARIABLE_T IDENT { ins_SymbolT($<var_type>1,$<s_var>2,address++,yylineno,"-",scope);  func_sig_array[func_sig_counter++]=$<var_type>1; }
    | VARIABLE_T IDENT '[' ']' { ins_SymbolT($<var_type>1,$<s_var>2,address++,yylineno,"-",scope); func_sig_array[func_sig_counter++]=($<var_type>1+12); }
    | ExprPlus
;

/* Scope */
StmtList 
    : StmtList Stmt
    | Stmt
;

Stmt
    : ';'
    | RegularDefineStmt ';'
    | COUT CoutParmListStmt ';'
    | IF '(' Expr ')' { token_IFELSE(1); } IfElseStmt { token_IFELSE(2); } Elsestmt { token_IFELSE(3); }
    | WHILE { fprintf(jasminFile, "W_alpha_%d:\n",++while_count); } '(' Expr { syT=crt_SymbolT(++scope); } ')' { fprintf(jasminFile, "\tifeq W_beta_%d\n",while_count); } '{' StmtList '}' { fprintf(jasminFile, "\tgoto W_alpha_%d\n",while_count); fprintf(jasminFile, "W_beta_%d:\n",while_count); }
    | FOR { syT=crt_SymbolT(++scope); stack_for[++stack_for_counter]=loopNum;} '(' ForStmt ')' '{' StmtList '}' { fprintf(jasminFile, "goto f_beta_%d\n",stack_for[stack_for_counter]); fprintf(jasminFile, "f_Delta_%d:\n",stack_for[stack_for_counter--]); }
    | BREAK ';' { fprintf(jasminFile, "\tgoto W_beta_%d\n",while_count); }
    | CONTINUE ';'
    | RETURN ';'
    | RETURN Expr ';'
;

Elsestmt
    : ELSE IfElseStmt
    |
;

RegularDefineStmt
    : VARIABLE_T StmtParameter { if($<var_type>1!=1){ cur_value_type=$<var_type>1; } stack_type[type_no++]=cur_value_type; }
    | VARIABLE_T IDENT ArrayDefStmt { array_counter=0; if(array_dim==0) { fprintf(jasminFile, "newarray %s\n",change_num_to_type($<var_type>1)); } else {fprintf(jasminFile, "multianewarray [[I %d\n",array_dim); array_dim=0;} ins_SymbolT($<var_type>1,$<s_var>2,address++,yylineno,"-",scope); fprintf(jasminFile, "\tastore %d\n",find_ident_address($<s_var>2));}
    | VARIABLE_T IDENT ArrayDefStmt { array_counter=0; if(array_dim==0) { fprintf(jasminFile, "newarray %s\n",change_num_to_type($<var_type>1)); } else {fprintf(jasminFile, "multianewarray [[I %d\n",array_dim); array_dim=0;} }VAL_ASSIGN ArrayExpression { ins_SymbolT($<var_type>1,$<s_var>2,address++,yylineno,"-",scope); fprintf(jasminFile, "\tastore %d\n",find_ident_address($<s_var>2)); }
    | Expr
;

CoutParmListStmt
    : CoutParmListStmt {fprintf(jasminFile, "\n\tgetstatic java/lang/System/out Ljava/io/PrintStream;\n");}  SHL ExprPlus {fprint_funcsig();}
    | SHL {fprintf(jasminFile, "\n\tgetstatic java/lang/System/out Ljava/io/PrintStream;\n");} ExprPlus {fprint_funcsig();}
;

IfElseStmt
    : Stmt
    | { syT=crt_SymbolT(++scope); } '{' StmtList '}' { scope--; }
;

ForStmt
    : ForInit { token_FOR(1); } ';' Expr ';' { token_FOR(2); } Expr { token_FOR(3); }
    | RegularDefineStmt ':' Expr
;


ForInit
    : RegularDefineStmt
    |
;

ArrayDefStmt
    : ArrayStmt '[' ArrayVariable ']' {array_dim = 2;}
    | '[' ArrayVariable ']'
;

ArrayStmt
    : ArrayStmt { fprintf(jasminFile, "\taaload\n"); } '[' ArrayVariable ']'
    | '[' ArrayVariable ']'
;

ArrayVariable
    : IDENT { if(strcmp($<s_var>1,"endl")==0){ cur_value_type=11; change_line=1; } else{ cur_value_type = find_ident_type($<s_var>1); cur_addr=find_ident_address($<s_var>1); fprintf_type(); fprintf(jasminFile, "load %d\n",cur_addr); }}
    | INT_LIT { fprintf(jasminFile, "\tldc %d\n",$<i_var>1); }
;

ArrayExpression
    : '{' ArrayInsideExpression '}'
;

ArrayInsideExpression
    : ArrayInsideExpression ','{fprintf(jasminFile, "\tdup\n");fprintf(jasminFile, "\tldc %d\n",array_counter++);} ExprPlus { fprintf_type(); fprintf(jasminFile, "astore\n"); }
    | {fprintf(jasminFile, "\tdup\n"); fprintf(jasminFile, "\tldc %d\n",array_counter++);} ExprPlus { fprintf_type(); fprintf(jasminFile, "astore\n"); }
    |
;

StmtParameter
    : StmtParameter ',' StmtParameterList
    | StmtParameterList
;

StmtParameterList
    : IDENT VAL_ASSIGN ExprRightSide {fprintf_type(); fprintf(jasminFile, "store %d\n",address); ins_SymbolT(type_no,$<s_var>1,address++,yylineno,"-",scope); }
    | IDENT { ins_SymbolT(type_no,$<s_var>1,address++,yylineno,"-",scope); }
;

Expr
    : ExprLeftSide VAL_ASSIGN Expr { if(cur_value_type!=find_ident_type_inAddr(store_addr)) { transformIdentType(find_ident_type_inAddr(store_addr)); } fprintf_type(); if(array_mode==1){ fprintf(jasminFile, "astore\n"); array_mode=0;} else { fprintf(jasminFile, "store %d\n",store_addr); } }
    | ExprLeftSide { fprintf_type(); fprintf(jasminFile, "load %d\n",store_addr); } ADD_ASSIGN Expr { fprintf_type(); fprintf(jasminFile, "add\n"); } { fprintf_type(); fprintf(jasminFile, "store %d\n",store_addr); }
    | ExprLeftSide { fprintf_type(); fprintf(jasminFile, "load %d\n",store_addr); } SUB_ASSIGN Expr { fprintf_type(); fprintf(jasminFile, "sub\n"); } { fprintf_type(); fprintf(jasminFile, "store %d\n",store_addr); }
    | ExprLeftSide { fprintf_type(); fprintf(jasminFile, "load %d\n",store_addr); } MUL_ASSIGN Expr { fprintf_type(); fprintf(jasminFile, "mul\n"); } { fprintf_type(); fprintf(jasminFile, "store %d\n",store_addr); }
    | ExprLeftSide { fprintf_type(); fprintf(jasminFile, "load %d\n",store_addr); } DIV_ASSIGN Expr { fprintf_type(); fprintf(jasminFile, "div\n"); } { fprintf_type(); fprintf(jasminFile, "store %d\n",store_addr); }
    | ExprLeftSide { fprintf_type(); fprintf(jasminFile, "load %d\n",store_addr); } REM_ASSIGN Expr { fprintf_type(); fprintf(jasminFile, "rem\n"); } { fprintf_type(); fprintf(jasminFile, "store %d\n",store_addr); }
    | ExprLeftSide { fprintf_type(); fprintf(jasminFile, "load %d\n",store_addr); } BAN_ASSIGN Expr { fprintf_type(); fprintf(jasminFile, "and\n"); } { fprintf_type(); fprintf(jasminFile, "store %d\n",store_addr); }
    | ExprLeftSide { fprintf_type(); fprintf(jasminFile, "load %d\n",store_addr); } BOR_ASSIGN Expr { fprintf_type(); fprintf(jasminFile, "or\n"); } { fprintf_type(); fprintf(jasminFile, "store %d\n",store_addr); }
    | ExprLeftSide { fprintf_type(); fprintf(jasminFile, "load %d\n",store_addr); } SHR_ASSIGN Expr { fprintf_type(); fprintf(jasminFile, "shr\n"); } { fprintf_type(); fprintf(jasminFile, "store %d\n",store_addr); }
    | ExprLeftSide { fprintf_type(); fprintf(jasminFile, "load %d\n",store_addr); } SHL_ASSIGN Expr { fprintf_type(); fprintf(jasminFile, "shl\n"); } { fprintf_type(); fprintf(jasminFile, "store %d\n",store_addr); }
    | ExprRightSide
;

ExprLeftSide
    : IDENT { store_addr=find_ident_address($<s_var>1); cur_value_type=find_ident_type($<s_var>1); }
    | IDENT { array_mode=1; fprintf(jasminFile, "aload %d\n",find_ident_address($<s_var>1)); } ArrayStmt {;}
    | '(' Expr ')' {;}
;

ExprRightSide
    : ExprRightSide LOR ExprDoubleAnd { fprintf_type(); fprintf(jasminFile, "or ;\n" );}
    | ExprDoubleAnd
;

ExprDoubleAnd
    : ExprDoubleAnd LAN ExprOr { fprintf_type(); fprintf(jasminFile, "and ;\n" ); }
    | ExprOr
;

ExprOr
    : ExprOr BOR ExprXor { fprintf_type(); fprintf(jasminFile, "or ;\n" ); }
    | ExprXor
;

ExprXor
    : ExprXor BXO ExprAnd { fprintf_type(); fprintf(jasminFile, "xor\n" ); }
    | ExprAnd
;

ExprAnd
    : ExprAnd BAN ExprSame { fprintf_type(); fprintf(jasminFile, "and\n" ); }
    | ExprSame
;

ExprSame
    : ExprSame EQL ExprBigOrSmall { token_EQLNEQNOT(1); }
    | ExprSame NEQ ExprBigOrSmall { token_EQLNEQNOT(2); }
    | ExprBigOrSmall
;

ExprBigOrSmall
    : ExprBigOrSmall GTR ExprBitwise { token_GTRGEQLEQLES(1); }
    | ExprBigOrSmall GEQ ExprBitwise { token_GTRGEQLEQLES(2); }
    | ExprBigOrSmall LEQ ExprBitwise { token_GTRGEQLEQLES(3); }
    | ExprBigOrSmall LES ExprBitwise { token_GTRGEQLEQLES(4); }
    | ExprBitwise
;

ExprBitwise
    : ExprBitwise SHR ExprPlus { fprintf(jasminFile, "\tishr ;\n" ); }
    | ExprBitwise SHL ExprPlus { fprintf(jasminFile, "\tishl ;\n" ); }
    | ExprPlus
;

ExprPlus
    : ExprPlus ADD ExprCross{ fprintf_type(); fprintf(jasminFile, "add ;\n" ); }
    | ExprPlus SUB ExprCross{ fprintf_type(); fprintf(jasminFile, "sub ;\n" ); }
    | ExprCross
;

ExprCross
    : ExprCross MUL ExprFrontUnary { fprintf_type(); fprintf(jasminFile, "mul ;\n" ); }
    | ExprCross DIV ExprFrontUnary { fprintf_type(); fprintf(jasminFile, "div ;\n" ); }
    | ExprCross REM ExprFrontUnary { fprintf_type(); fprintf(jasminFile, "rem ;\n" );}
    | ExprFrontUnary
;

ExprFrontUnary
    : NOT ExprFrontUnary { token_EQLNEQNOT(3); }
    | BNT ExprFrontUnary { token_BNT(); }
    | ADD ExprFrontUnary {;}
    | SUB ExprFrontUnary { if(cur_value_type==7) fprintf(jasminFile, "\tineg ;\n" ); if(cur_value_type==9) fprintf(jasminFile, "\tfneg ;\n" );}
    | '(' VARIABLE_T ')' { cur_addr=-1; } ExprPostfix {cur_value_type=$<var_type>2; transformIdentType(cur_value_type);  }
    | ExprPostfix
;

ExprPostfix
    : ExprValue INC_ASSIGN { token_INCDEC(1); }
    | ExprValue DEC_ASSIGN { token_INCDEC(2); }
    | ExprValue
;

ExprValue
    : INT_LIT { fprintf(jasminFile, "\tldc %d ;\n",$<i_var>1 ); lineType=cur_value_type=7; }
    | FLOAT_LIT { fprintf(jasminFile, "\tldc %f ;\n",$<f_var>1 ); lineType=cur_value_type=9; }
    | BOOL_LIT { fprintf(jasminFile, "\tldc %d ;\n",$<b_var>1 ); lineType=cur_value_type=3; }
    | CHAR_LIT { fprintf(jasminFile, "\tldc \"%c\" ;\n",$<c_var>1 ); lineType=cur_value_type=11; }
    | STR_LIT { fprintf(jasminFile, "\tldc \"%s\" ;\n",$<s_var>1 ); lineType=cur_value_type=11; }
    | IDENT '(' FunctionParameterStmtList ')' { fprintf(jasminFile, "\tinvokestatic Main/%s%s\n",$<s_var>1,find_call($<s_var>1) ); cur_value_type=find_func_type(find_call($<s_var>1));}
    | IDENT { if(strcmp($<s_var>1,"calculate_pi")==0){ cur_value_type=9; }else if(strcmp($<s_var>1,"endl")==0){ cur_value_type=11; change_line=1; }else{ cur_value_type = find_ident_type($<s_var>1); cur_addr=find_ident_address($<s_var>1); fprintf_type(); fprintf(jasminFile, "load %d\n",cur_addr); } }
    | IDENT { fprintf(jasminFile, "\taload %d\n",find_ident_address($<s_var>1)); } ArrayStmt { cur_value_type = find_ident_type($<s_var>1); fprintf_type(); fprintf(jasminFile, "aload\n"); }
    | '(' Expr ')' {;}
;


%%
/* C code section */
void init_jasminFile(){
    jasminFile = fopen("build/Main.j", "w");

    fprintf(jasminFile, ".source Main.j\n.class public Main\n.super java/lang/Object\n");
}

void writeFunc_jasminFile(char* name , char* func_sig){
    fprintf(jasminFile, "\n.method public static %s%s\n",name,func_sig);
    fprintf(jasminFile, ".limit stack 100\n.limit locals 100\n");
}

void leaveFunc_jasminFile(int type,char* id){
    if( (type==3 || type==7 || type==9) && (strcmp(id,"main") != 0) ){
        fprintf_type(); 
    }
    fprintf(jasminFile, "return\n.end method\n");
}

SymbolT* crt_SymbolT(int insertScope){
    if(syT != NULL){
        return syT;
    }
    else{
        int size = sizeof(SymbolT);
        return (SymbolT*)malloc(size);
    }
}

void ins_SymbolT(int type,char* name,int addr,int lineno,char* func_sig,int currentScope){ 
    int size = sizeof(SymbolTColumn);
    SymbolTColumn* column = (SymbolTColumn*)malloc(size);
    column->index = find_index(currentScope);
    column->scope_level = currentScope;
    column->name = name;
    column->type = type;
    column->addr = addr;
    column->lineno = lineno;
    column->func_sig = func_sig;
    column->print = 0;
    column->next = NULL;

    if(column->type == 1){
        column->type = cur_value_type;
    }

    if(tailColumn != NULL){
        tailColumn->next = column;
    }
    else{
        syT->head = column;
    }
    tailColumn = column;
} 


char* find_call(char* id){
    SymbolTColumn* currentColumn = syT->head;
    while(currentColumn != NULL){
        if( strcmp(currentColumn->name,id) == 0 ){
            return currentColumn->func_sig;
        }
        else{
            currentColumn = currentColumn->next;
        }
    }
    return "-";
}

int find_index(int currentScope){
    int index=0;
    SymbolTColumn* currentColumn = syT->head;
    while(currentColumn != NULL){
        if( currentColumn->scope_level == currentScope && currentColumn->print == 0 ){
            index++;
        }
        currentColumn = currentColumn->next;
    }
    return index;
}

int find_ident_address(char* id){
    int maximum_scope = -10;
    int ans = -1;
    SymbolTColumn* currentColumn = syT->head;
    while(currentColumn != NULL){
        if( strcmp(currentColumn->name,id) == 0 && currentColumn->print == 0 && currentColumn->scope_level > maximum_scope && currentColumn->addr >= 0 ){
            maximum_scope = currentColumn->scope_level;
            ans = currentColumn -> addr;
        }
        currentColumn = currentColumn->next;
    }
    return ans;
}

int find_func_type(char* sig){
    char funcType = sig[strlen(sig)-1];
    
    if(funcType == 'Z') return 3;
    else if(funcType == 'I') return 7;
    else if(funcType == 'F') return 9;
    else return 11;
}

int find_ident_type(char* id){
    SymbolTColumn* currentColumn = syT->head;
    while(currentColumn != NULL){
        if( strcmp(currentColumn->name,id) == 0 && currentColumn->type!=0 ){
            return currentColumn -> type;
        }
        else{
            currentColumn = currentColumn->next;
        }
    }
    return 9;
}

int find_ident_type_inAddr(int scope_address){
    SymbolTColumn* currentColumn = syT->head;
    while(currentColumn != NULL){
        if(currentColumn->type >= 13){
            currentColumn->type = stack_type[currentColumn->type];
        }
        if( currentColumn->addr == scope_address && currentColumn->print == 0 ){
            return currentColumn->type;
        }
        else{
            currentColumn = currentColumn->next;
        }
    }
    return -1;
}

void ins_func_sig(char* id){
    int count=-1;
    SymbolTColumn* currentColumn = syT->head;
    while(currentColumn != NULL){
        if( currentColumn->type == 0 && currentColumn->print == 0 ){
            count++;
        }

        if( strcmp(currentColumn->name,id) == 0 ){
            currentColumn->func_sig = tempFuncSig[count];
            break;
        }
        else{
            currentColumn = currentColumn->next;
        }
    }
}

void construct_func(char* id){
    strcpy(tempFuncSig[tempFuncSig_counter],"(");
    for(int i=0 ; i < func_sig_counter ; i++){
        if(func_sig_array[i] > 12){
            strcat(tempFuncSig[tempFuncSig_counter],"[");
            func_sig_array[i] -= 12;
        }

        if( (i == func_sig_counter-1 && strcmp("main",id) == 0) || func_sig_array[i] == 2){
            strcat(tempFuncSig[tempFuncSig_counter],"V");
        }
        else if(func_sig_array[i] == 3){
            strcat(tempFuncSig[tempFuncSig_counter],"Z");
        }
        else if(func_sig_array[i] == 7){
            strcat(tempFuncSig[tempFuncSig_counter],"I");
        }
        else if(func_sig_array[i] == 9){
            strcat(tempFuncSig[tempFuncSig_counter],"F");
        }
        else if(func_sig_array[i] == 11){
            strcat(tempFuncSig[tempFuncSig_counter],"Ljava/lang/String;");
        }

        if(i == func_sig_counter-2)   strcat(tempFuncSig[tempFuncSig_counter],")");
    }
    tempFuncSig_counter++;
    func_sig_counter = 0;
}

void fprint_funcsig(){
    if(cur_value_type>=13) cur_value_type = stack_type[cur_value_type];
    if(change_line==1) fprintf(jasminFile, "\tldc \"\\n\"\n");
    fprintf(jasminFile, "\tinvokevirtual java/io/PrintStream/print(");
    if(cur_value_type <= 2)         fprintf(jasminFile, "V");
    else if(cur_value_type == 3)    fprintf(jasminFile, "Z");
    else if(cur_value_type == 4)    fprintf(jasminFile, "B");
    else if(cur_value_type == 5)    fprintf(jasminFile, "C");
    else if(cur_value_type == 6)    fprintf(jasminFile, "S");
    else if(cur_value_type == 7)    fprintf(jasminFile, "I");
    else if(cur_value_type == 8)    fprintf(jasminFile, "J");
    else if(cur_value_type == 9)    fprintf(jasminFile, "F");
    else if(cur_value_type == 10)   fprintf(jasminFile, "D");
    else if(cur_value_type == 11)   fprintf(jasminFile, "Ljava/lang/String;");
    fprintf(jasminFile, ")V\n\n");
    change_line=0;
}

char* change_num_to_type(int currentType){
    if( currentType == 1 )         return "auto";
    else if( currentType == 2 )    return "void";
    else if( currentType == 3 )    return "bool";
    else if( currentType == 4 )    return "byte";
    else if( currentType == 5 )    return "char";
    else if( currentType == 6 )    return "short";
    else if( currentType == 7 )    return "int";
    else if( currentType == 8 )    return "long";
    else if( currentType == 9 )    return "float";
    else if( currentType == 10 )   return "double";
    else if( currentType == 11 )   return "string";
    else if( currentType == 12 )   return "function";
    else                           return change_num_to_type(stack_type[currentType]);
}

void fprintf_type(){
    if(cur_value_type>=13) cur_value_type = stack_type[cur_value_type];

    if(cur_value_type==3 || cur_value_type==7)  fprintf(jasminFile, "\ti");
    else if(cur_value_type==9)                  fprintf(jasminFile, "\tf");
    else if(cur_value_type==11)                 fprintf(jasminFile, "\ta");
    else                                        fprintf(jasminFile, "wrong %d",cur_value_type);
}

void token_GTRGEQLEQLES(int action){
    if(cur_value_type>=13) cur_value_type = stack_type[cur_value_type];
    
    if(action == 1 && cur_value_type==7){
        fprintf(jasminFile, "\tif_icmpgt Loop_0_%d\n",loopNum);
    }
    else if(action == 1 && cur_value_type == 9 ){
        fprintf(jasminFile, "\tfcmpg  ;\n");
        fprintf(jasminFile, "\tifgt Loop_0_%d\n",loopNum);
        
    }
    else if(action == 2) fprintf(jasminFile, "\tif_icmpge Loop_0_%d\n",loopNum);
    else if(action == 3) fprintf(jasminFile, "\tif_icmple Loop_0_%d\n",loopNum);
    else if(action == 4) fprintf(jasminFile, "\tif_icmplt Loop_0_%d\n",loopNum);

    fprintf(jasminFile, "\ticonst_0\n");
    fprintf(jasminFile, "\tgoto Loop_1_%d\n",loopNum);
    fprintf(jasminFile, "Loop_0_%d:\n",loopNum);
    fprintf(jasminFile, "\ticonst_1\n");
    fprintf(jasminFile, "Loop_1_%d:\n",loopNum++);
}

void token_EQLNEQNOT(int action){
    if(action == 1)      fprintf(jasminFile, "\tif_icmpeq Loop_0_%d\n",loopNum);
    else if(action == 2) fprintf(jasminFile, "\tif_icmpne Loop_0_%d\n",loopNum);
    else if(action == 3) fprintf(jasminFile, "\tifne Loop_0_%d\n",loopNum);

    fprintf(jasminFile, "\ticonst_0\n");
    fprintf(jasminFile, "\tgoto Loop_1_%d\n",loopNum);
    fprintf(jasminFile, "Loop_0_%d:\n",loopNum);
    fprintf(jasminFile, "\ticonst_1\n");
    fprintf(jasminFile, "Loop_1_%d:\n",loopNum++);
}

void token_INCDEC(int action){
    fprintf(jasminFile, "\tldc 1\n");

    fprintf_type();
    if(action==1) fprintf(jasminFile, "add\n");
    else if(action==2) fprintf(jasminFile, "sub\n");

    fprintf(jasminFile, "\tistore %d\n",cur_addr);
}

void token_IFELSE(int action){
    if(action==1){
        stack_if[++stack_if_counter]=(loopNum++);
        fprintf(jasminFile, "\tifeq If_alpha_%d\n",stack_if[stack_if_counter]); 
    }
    else if(action==2){
        if(func_if==1){
            fprintf(jasminFile, "\tireturn\n");
            func_if=0;
        }
        fprintf(jasminFile, "\tgoto If_belta_%d\n",stack_if[stack_if_counter]);
        fprintf(jasminFile, "If_alpha_%d:\n",stack_if[stack_if_counter]);
    }
    else{
        fprintf(jasminFile, "If_belta_%d:\n",stack_if[stack_if_counter--]);
    }
}

void token_FOR(int action){
    if(action==1){
        fprintf(jasminFile, "f_Alpha_%d:\n",stack_for[stack_for_counter]);
    }
    else if(action==2){
        fprintf(jasminFile, "ifeq f_Delta_%d\n",stack_for[stack_for_counter]);
        fprintf(jasminFile, "\tgoto f_Gamma_%d\n",stack_for[stack_for_counter]);
        fprintf(jasminFile, "f_beta_%d:\n",stack_for[stack_for_counter]);
    }
    else{
        fprintf(jasminFile, "\tgoto f_Alpha_%d\n",stack_for[stack_for_counter]);
        fprintf(jasminFile, "f_Gamma_%d:\n",stack_for[stack_for_counter]);
    }
}

void token_BNT(){
    fprintf(jasminFile, "\ticonst_m1\n");
    fprintf(jasminFile, "\tixor\n");
}

void transformIdentType(int type){
    if(type==7){
        fprintf(jasminFile, "\tf2i\n");
        cur_value_type = type;
    }
    else if(type==9){
        fprintf(jasminFile, "\ti2f\n");
        cur_value_type = type;
    }
}