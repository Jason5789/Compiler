/* Definition section */
%{
    #include "compiler_common.h"
    #include "compiler_util.h"
    #include "main.h"

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
    int stack_type[30];
    
    int stack_cout[30];
    int cout_counter = 0;
    int type_no = 10;
    int array_length = 0;
    int cur_value_type = 1;
    int func_sig_array[30];
    int func_sig_counter = 0;
    int tempFuncSig_counter = 0;
    char tempFuncSig[10][80];
    SymbolT* syT = NULL;
    SymbolTColumn* tailColumn = NULL;

    SymbolT* crt_SymbolT(int insertScope);
    void ins_SymbolT(int type,char* name,int addr,int lineno,char* func_sig,int currentScope);
    void print_SymbolT(SymbolTColumn* currentColumn, int currentScope);
    void print_ident(char* name,int address);
    void print_cout();
    char* change_num_to_type(int type);
    char* find_call(char* id);
    int find_index(int currentScope);
    int find_ident_type(char* id);
    int find_ident_address(char* id);
    void ins_func_sig(char *id);  
    void construct_func();
%}

/* Variable or self-defined structure */
%union{
    ObjectType var_type;

    bool b_var;
    int i_var;
    float f_var;
    char *s_var;

    Object object_var;
}

/* Token without return */
%token COUT
%token SHR SHL BAN BOR BNT BXO ADD SUB MUL DIV REM NOT GTR LES GEQ LEQ EQL NEQ LAN LOR
%token VAL_ASSIGN ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN DIV_ASSIGN REM_ASSIGN BAN_ASSIGN BOR_ASSIGN BXO_ASSIGN SHR_ASSIGN SHL_ASSIGN INC_ASSIGN DEC_ASSIGN
%token IF ELSE FOR WHILE RETURN BREAK CONTINUE

/* Token with return, which need to sepcify type */
%token <var_type> VARIABLE_T
%token <s_var> IDENT
%token <i_var> INT_LIT
%token <f_var> FLOAT_LIT
%token <b_var> BOOL_LIT
%token <s_var> STR_LIT

/* Nonterminal with return, which need to sepcify type */
%type <object_val> Expr
%type <object_val> ExprRightSide
%type <object_val> ExprDoubleAnd
%type <object_val> ExprOr
%type <object_val> ExprXor
%type <object_val> ExprAnd
%type <object_val> ExprSame
%type <object_val> ExprBigOrSmall
%type <object_val> ExprBitwise
%type <object_val> ExprPlus
%type <object_val> ExprCross
%type <object_val> ExprFrontUnary
%type <object_val> ExprPostfix
%type <object_val> ExprValue
%type <object_val> ExprLeftSide

%left ADD SUB
%left MUL DIV REM

/* Yacc will start at this nonterminal */
%start Program

%%
/* Grammar section */

Program
    : { syT=crt_SymbolT(++scope);} GlobalStmtList { print_SymbolT(syT->head,scope--); }
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
    : VARIABLE_T IDENT { printf("func: %s\n",$<s_var>2); ins_SymbolT(0,$<s_var>2,-1,yylineno,"unknown",scope); } '(' { syT=crt_SymbolT(++scope); } FunctionParameterStmtList ')' { func_sig_array[func_sig_counter++]=$<var_type>1; construct_func(); ins_func_sig($<s_var>2); } '{' StmtList '}' { print_SymbolT(syT->head,scope--); }
;

FunctionParameterStmtList
    : FunctionParameterStmtList ',' FunctionParameterStmt
    | FunctionParameterStmt
    |
;

FunctionParameterStmt
    : VARIABLE_T IDENT { ins_SymbolT($<var_type>1,$<s_var>2,address++,yylineno,"-",scope); func_sig_array[func_sig_counter++]=$<var_type>1; }
    | VARIABLE_T IDENT '[' ']' { ins_SymbolT($<var_type>1,$<s_var>2,address++,yylineno,"-",scope); func_sig_array[func_sig_counter++]=($<var_type>1+10); }
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
    | COUT CoutParmListStmt ';' { print_cout(); }
    | IF '(' Expr ')' { printf("IF\n"); } IfElseStmt
    | ELSE { printf("ELSE\n"); } IfElseStmt
    | WHILE { printf("WHILE\n"); } '(' Expr { syT=crt_SymbolT(++scope); } ')' '{' StmtList '}' { print_SymbolT(syT->head,scope--);} 
    | FOR { printf("FOR\n"); syT=crt_SymbolT(++scope); } '(' ForStmt ')' '{' StmtList '}' { print_SymbolT(syT->head,scope--);} 
    | BREAK ';' { printf("BREAK\n"); }
    | CONTINUE ';' { printf("CONTINUE\n"); }
    | RETURN ';' { printf("RETURN\n"); }
    | RETURN Expr ';' { printf("RETURN\n"); }
;

RegularDefineStmt
    : VARIABLE_T StmtParameter { if($<var_type>1!=1){ cur_value_type=$<var_type>1; } stack_type[type_no++]=cur_value_type; }
    | VARIABLE_T IDENT ArrayStmt { ins_SymbolT($<var_type>1,$<s_var>2,address++,yylineno,"-",scope); }
    | VARIABLE_T IDENT ArrayStmt VAL_ASSIGN ArrayExpression { ins_SymbolT($<var_type>1,$<s_var>2,address++,yylineno,"-",scope); }
    | Expr
;

CoutParmListStmt
    : CoutParmListStmt SHL ExprPlus { stack_cout[cout_counter++]=cur_value_type; }
    | SHL ExprPlus { stack_cout[cout_counter++]=cur_value_type; }
;

IfElseStmt
    : Stmt
    | { syT=crt_SymbolT(++scope); } '{' StmtList '}' { print_SymbolT(syT->head,scope--);}
;

ForStmt
    : ForInit ';' Expr ';' Expr
    | RegularDefineStmt ':' Expr
;

ForInit
    : RegularDefineStmt
    |
;

ArrayStmt
    : ArrayStmt '[' ArrayVariable ']'
    | '[' ArrayVariable ']'
;

ArrayVariable
    : IDENT { print_ident($<s_var>1,find_ident_address($<s_var>1)); }
    | INT_LIT { printf("INT_LIT %d\n",$<i_var>1); }
;

ArrayExpression
    : '{' ArrayInsideExpression '}' { printf("create array: %d\n",array_length); array_length=0; }
;

ArrayInsideExpression
    : ArrayInsideExpression ',' ExprPlus { array_length++; }
    | ExprPlus { array_length++; }
    |
;

StmtParameter
    : StmtParameter ',' StmtParameterList
    | StmtParameterList
;

StmtParameterList
    : IDENT VAL_ASSIGN ExprRightSide { ins_SymbolT(type_no,$<s_var>1,address++,yylineno,"-",scope); }
    | IDENT { ins_SymbolT(type_no,$<s_var>1,address++,yylineno,"-",scope); }
;

Expr
    : ExprLeftSide VAL_ASSIGN Expr { printf("EQL_ASSIGN\n"); }
    | ExprLeftSide ADD_ASSIGN Expr { printf("ADD_ASSIGN\n"); }
    | ExprLeftSide SUB_ASSIGN Expr { printf("SUB_ASSIGN\n"); }
    | ExprLeftSide MUL_ASSIGN Expr { printf("MUL_ASSIGN\n"); }
    | ExprLeftSide DIV_ASSIGN Expr { printf("DIV_ASSIGN\n"); }
    | ExprLeftSide REM_ASSIGN Expr { printf("REM_ASSIGN\n"); }
    | ExprLeftSide SHR_ASSIGN Expr { printf("SHR_ASSIGN\n"); }
    | ExprLeftSide SHL_ASSIGN Expr { printf("SHL_ASSIGN\n"); }
    | ExprLeftSide BAN_ASSIGN Expr { printf("BAN_ASSIGN\n"); }
    | ExprLeftSide BOR_ASSIGN Expr { printf("BOR_ASSIGN\n"); }
    | ExprRightSide
;

ExprRightSide
    : ExprRightSide LOR ExprDoubleAnd { printf("LOR\n"); }
    | ExprDoubleAnd
;

ExprDoubleAnd
    : ExprDoubleAnd LAN ExprOr { printf("LAN\n"); }
    | ExprOr
;

ExprOr
    : ExprOr BOR ExprXor { printf("BOR\n"); }
    | ExprXor
;

ExprXor
    : ExprXor BXO ExprAnd { printf("BXO\n"); }
    | ExprAnd
;

ExprAnd
    : ExprAnd BAN ExprSame { printf("BAN\n"); }
    | ExprSame
;

ExprSame
    : ExprSame EQL ExprBigOrSmall { printf("EQL\n"); }
    | ExprSame NEQ ExprBigOrSmall { printf("NEQ\n"); }
    | ExprBigOrSmall
;

ExprBigOrSmall
    : ExprBigOrSmall GEQ ExprBitwise { printf("GEQ\n"); }
    | ExprBigOrSmall GTR ExprBitwise { printf("GTR\n"); }
    | ExprBigOrSmall LEQ ExprBitwise { printf("LEQ\n"); }
    | ExprBigOrSmall LES ExprBitwise { printf("LES\n"); }
    | ExprBitwise
;

ExprBitwise
    : ExprBitwise SHR ExprPlus { printf("SHR\n"); }
    | ExprBitwise SHL ExprPlus { printf("SHL\n"); }
    | ExprPlus
;

ExprPlus
    : ExprPlus ADD ExprCross { printf("ADD\n"); }
    | ExprPlus SUB ExprCross { printf("SUB\n"); }
    | ExprCross
;

ExprCross
    : ExprCross MUL ExprFrontUnary { printf("MUL\n"); }
    | ExprCross DIV ExprFrontUnary { printf("DIV\n"); }
    | ExprCross REM ExprFrontUnary { printf("REM\n"); }
    | ExprFrontUnary
;

ExprFrontUnary
    : NOT ExprFrontUnary { printf("NOT\n"); }
    | BNT ExprFrontUnary { printf("BNT\n"); }
    | ADD ExprFrontUnary { printf("ADD\n"); }
    | SUB ExprFrontUnary { printf("NEG\n"); }
    | '(' VARIABLE_T ')' ExprPostfix { printf("Cast to %s\n", change_num_to_type($<var_type>2)); }
    | ExprPostfix
;

ExprPostfix
    : ExprValue INC_ASSIGN { printf("INC_ASSIGN\n"); }
    | ExprValue DEC_ASSIGN { printf("DEC_ASSIGN\n"); }
    | ExprValue
;

ExprValue
    : INT_LIT { printf("INT_LIT %d\n",$<i_var>1); cur_value_type = 4; }
    | FLOAT_LIT { printf("FLOAT_LIT %f\n",$<f_var>1); cur_value_type = 6; }
    | BOOL_LIT { printf("BOOL_LIT "); ($<b_var>1==0)?printf("FALSE\n"):printf("TRUE\n"); cur_value_type = 8; }
    | STR_LIT { printf("STR_LIT \"%s\"\n",$<s_var>1); cur_value_type = 9; }
    | IDENT '(' FunctionParameterStmtList ')' { print_ident($<s_var>1,-1); printf("call: %s%s\n",$<s_var>1,find_call($<s_var>1)); }
    | ExprLeftSide
;

ExprLeftSide
    : IDENT { print_ident($<s_var>1,find_ident_address($<s_var>1)); cur_value_type = find_ident_type($<s_var>1); }
    | IDENT ArrayStmt { print_ident($<s_var>1,find_ident_address($<s_var>1)); cur_value_type = find_ident_type($<s_var>1); }
    | '(' Expr ')' { pushFunInParm(&$<object_var>1); }
;

%%
/* C code section */
SymbolT* crt_SymbolT(int insertScope){
    printf("> Create symbol table (scope level %d)\n",insertScope);

    if(syT != NULL){
        return syT;
    }
    else{
        int size = sizeof(SymbolT);
        return (SymbolT*)malloc(size);
    }
}

void ins_SymbolT(int type,char* name,int addr,int lineno,char* func_sig,int currentScope){
    printf("> Insert `%s` (addr: %d) to scope level %d\n",name,addr,currentScope);
    
    int size = sizeof(SymbolTColumn);
    SymbolTColumn* column = (SymbolTColumn*)malloc(size);
    column->scope_level = currentScope;
    column->index = find_index(currentScope);
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

void print_SymbolT(SymbolTColumn* currentColumn, int currentScope){
    printf("\n");
    printf("> Dump symbol table (scope level: %d)\n",currentScope);
    printf("%-10s%-20s%-10s%-10s%-10s%-10s\n","Index","Name","Type","Addr","Lineno","Func_sig");
    
    if(currentColumn != NULL){
        do{
            if( currentColumn->scope_level == currentScope && currentColumn->print == 0 ){
                currentColumn->print = 1;
                printf("%-10d%-20s%-10s%-10d%-10d%-10s\n", currentColumn->index, currentColumn->name, change_num_to_type(currentColumn->type), currentColumn->addr, currentColumn->lineno, currentColumn->func_sig);
            }
            currentColumn = currentColumn->next;
        }while(currentColumn != NULL);
    }
}

void print_ident(char* name,int address){
    printf("IDENT (name=%s, address=%d)\n",name,address);
}

void print_cout(){
    printf("cout"); 
    for(int i=0; i<cout_counter ; i++){
        if( stack_cout[i] > 9 )          stack_cout[i]=stack_type[stack_cout[i]];

        if( stack_cout[i] == 3 )         printf(" char");
        else if( stack_cout[i] == 4 )    printf(" int");
        else if( stack_cout[i] == 6 )    printf(" float");
        else if( stack_cout[i] == 8 )    printf(" bool");
        else if( stack_cout[i] == 9 )    printf(" string");
    }
    printf("\n");
    cout_counter = 0;
}

char* change_num_to_type(int currentType){
    if( currentType == 0 )         return "function";
    else if( currentType == 1 )    return "auto";
    else if( currentType == 2 )    return "void";
    else if( currentType == 3 )    return "char";
    else if( currentType == 4 )    return "int";
    else if( currentType == 5 )    return "long";
    else if( currentType == 6 )    return "float";
    else if( currentType == 7 )    return "double";
    else if( currentType == 8 )    return "bool";
    else if( currentType == 9 )    return "string";
    else                           return change_num_to_type(stack_type[currentType]);
}

char* find_call(char* id){
    SymbolTColumn* currentColumn = syT->head;
    while(currentColumn != NULL){
        if( strcmp(currentColumn->name,id) == 0 ){
            return currentColumn->func_sig;
        }
        currentColumn = currentColumn->next;
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

int find_ident_type(char* id){
    SymbolTColumn* currentColumn = syT->head;
    while(currentColumn != NULL){
        if( strcmp(currentColumn->name,id) == 0 ){
            return currentColumn -> type;
        }
        currentColumn = currentColumn->next;
    }
    return 9;
}

int find_ident_address(char* id){
    int max_scope = -10;
    int return_addr = -1;
    SymbolTColumn* currentColumn = syT->head;
    while(currentColumn != NULL){
        if( strcmp(currentColumn->name,id) == 0 && currentColumn->print == 0 && currentColumn->scope_level > max_scope && currentColumn->addr >= 0 ){
            max_scope = currentColumn->scope_level;
            return_addr = currentColumn -> addr;
        }
        currentColumn = currentColumn->next;
    }
    return return_addr;
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
        currentColumn = currentColumn->next;
    }
}

void construct_func(){
    strcpy(tempFuncSig[tempFuncSig_counter],"(");
    for(int i=0 ; i < func_sig_counter ; i++){
        if(func_sig_array[i] > 10){
            strcat(tempFuncSig[tempFuncSig_counter],"[");
            func_sig_array[i] -= 10;
        }

        if(func_sig_array[i] == 2)         strcat(tempFuncSig[tempFuncSig_counter],"V");
        else if(func_sig_array[i] == 4)    strcat(tempFuncSig[tempFuncSig_counter],"I");
        else if(func_sig_array[i] == 8)    strcat(tempFuncSig[tempFuncSig_counter],"B");
        else if(func_sig_array[i] == 9)    strcat(tempFuncSig[tempFuncSig_counter],"Ljava/lang/String;");

        if(i == func_sig_counter-2)        strcat(tempFuncSig[tempFuncSig_counter],")");
    }
    tempFuncSig_counter++;
    func_sig_counter = 0;
}