%{
  #define YY_NO_UNPUT
  // #define YYERROR_VERBOSE
  #include <stdio.h>
  #include <stdlib.h>
  #include <string>
  #include <string.h>
  #include <set>
  #include <map>
  #include <stdlib.h>
  #include <sstream>
  #include <algorithm>
  int yyerror(const char *s);
  int yylex(void);
  extern int currLine, currPos;
  extern FILE* yyin;
  extern char* yytext;

  bool hasMainFunction = false;
  bool hasErrors = false;
  std::string new_temp();
  std::string new_label();
  std::string new_enum();

  std::set<std::string> reserved {"FUNCTION", "BEGIN_PARAMS", "END_PARAMS", "BEGIN_LOCALS", "END_LOCALS", "BEGIN_BODY", "END_BODY", "INTEGER", "ARRAY", "ENUM",
  "OF", "IF", "THEN", "ENDIF", "ELSE", "WHILE", "DO", "BEGINLOOP", "ENDLOOP", "CONTINUE", "READ", "WRITE", "AND", "OR", "NOT", "TRUE", "FALSE", "RETURN", "SUB",
  "ADD", "MULT", "DIV", "MOD", "EQ", "NEQ", "LT", "GT", "LTE", "GTE", "SEMICOLON", "COLON", "COMMA", "L_PAREN", "R_PAREN", "L_SQUARE_BRACKET", "R_SQUARE_BRACKET", "ASSIGN",
  "program", "functions", "function", "declarations", "declaration", "identifiers", "statements", "statement", "vars", "bool_exp", "relation_and_exp", "relation_exp",
  "comp", "expression", "multiplicative_expression", "term", "expressions", "var", "ident"};
  std::set<std::string> funcs;
  std::map<std::string, std::string> varTemp;
  std::map<std::string, int> arrSize;

%}

%union{
  int num_val;
  char* id_val;
  struct S {
    char* code;
  } statement;
  struct E {
    char* place;
    char* code;
    bool arr;
  } expression;
}
%error-verbose

%start	prog_start

// Tokens are terminals
%token <id_val> IDENT
%token <num_val> NUMBER
%type <expression> function functions declarations declaration vars var expressions expression func_ident identifiers ident
%type <expression> bool_exp relation_and_exp relation_exp comp multiplicative_expression term
%type <statement> statements statement
%token FUNCTION BEGIN_PARAMS END_PARAMS BEGIN_LOCALS END_LOCALS BEGIN_BODY END_BODY INTEGER ARRAY ENUM OF IF THEN ENDIF ELSE WHILE DO BEGINLOOP ENDLOOP CONTINUE READ WRITE AND OR NOT TRUE FALSE RETURN
%token SUB ADD MULT DIV MOD
%token EQ NEQ LT GT LTE GTE
%token SEMICOLON COLON COMMA L_PAREN R_PAREN L_SQUARE_BRACKET R_SQUARE_BRACKET ASSIGN

// Items declared LATER have a HIGHER precedence
%right ASSIGN
%left OR
%left AND
%right NOT
%left GT GTE LT LTE EQ NEQ
%left ADD SUB
%left MULT DIV MOD
// %right UNARY_MINUS ????
%left L_SQUARE_BRACKET R_SQUARE_BRACKET
%left L_PAREN R_PAREN

%%
prog_start: functions {
    if (!hasErrors) {
      printf("%s", $1.code);
    } else {
      printf("\n");
    }
  }
  	;

functions: %empty {
    if (!hasMainFunction) {
      printf("Error on line %d: No main function declared!", currLine);
      hasErrors = true;
    }
  }
	| function functions {
    std::string temp;
    temp.append($1.code);
    temp.append($2.code);
    $$.code = strdup(temp.c_str());
  }
	;


function: FUNCTION func_ident SEMICOLON BEGIN_PARAMS declarations END_PARAMS BEGIN_LOCALS declarations END_LOCALS BEGIN_BODY statements END_BODY
	{
    std::string temp = "func ";
    temp.append($2.place);
    temp.append("\n");
    std::string s = $2.place;
    if (s == "main") {
      hasMainFunction = true;
    }
    temp.append($5.code);
    std::string decs = $5.code;
    int paramNumber = 0;
    int pos;
    // handle params in callee function 
    while (decs.find(".") != std::string::npos) {
      pos = decs.find(".");
      decs.replace(pos, 1, "=");
      std::string part = ", $" + std::to_string(paramNumber) + "\n";
      paramNumber++;
      decs.replace(decs.find("\n", pos), 1, part);
    }
    temp.append(decs);

    temp.append($8.code);

    std::string statements = $11.code;
    if (statements.find("continue") != std::string::npos) {
      printf("Error on line %d: continue statement not within a loop. %s", currLine, $2.place);
      hasErrors = true;
    }
    temp.append(statements);
    temp.append("endfunc\n\n");
    $$.code = strdup(temp.c_str());
    $$.place = strdup(s.c_str());
  };

declarations: %empty {
    $$.code = strdup("");
    $$.place = strdup("");
  }
	| declaration SEMICOLON declarations {
    std::string temp;
    temp.append($1.code);
    temp.append($3.code);
    $$.code = strdup(temp.c_str());
    $$.place = strdup("");
  };

func_ident: ident {
    if (funcs.find($1.place) != funcs.end()) {
      printf("function name %s is already declared.\n", $1.place);
    } else {
      funcs.insert($1.place);
    }
    $$.place = strdup($1.place);
    $$.code = strdup("");
  };

declaration: identifiers COLON INTEGER {
    size_t left = 0;
    size_t right = 0;
    std::string parse($1.place); // i|j|k if multiple declarations
    std::string temp;
    bool flag = false;
    while (!flag) {
      right = parse.find("|", left);
      temp.append(". ");
      if (right == std::string::npos) { // Only one declaration in string
        std::string ident = parse.substr(left, right);
        if (reserved.find(ident) != reserved.end()) {
          printf("Error on line %d: Identifier %s's name is a reserved word.\n", currLine, ident.c_str());
          hasErrors = true;
        }
        if (funcs.find(ident) != funcs.end() || varTemp.find(ident) != varTemp.end()) {
          printf("Error in line %d: Identifier %s is previously declared.\n", currLine, ident.c_str());
          hasErrors = true;
        } else {
          varTemp[ident] = ident;
          arrSize[ident] = 1;
        }
        temp.append(ident);
        flag = true;
      } 
      else { // Multiple declarations
        std::string ident = parse.substr(left, right-left);
        if (reserved.find(ident) != reserved.end()) {
          printf("Error in line %d: Identifier %s's name is a reserved word.\n", currLine, ident.c_str());
          hasErrors = true;
          // exit(1);
        }
        if (funcs.find(ident) != funcs.end() || varTemp.find(ident) != varTemp.end()) {
          printf("Error in line %d: Identifier %s is previously declared.\n", currLine, ident.c_str());
        } else {
          varTemp[ident] = ident;
          arrSize[ident] = 1;
        }
        temp.append(ident);
        left = right+1;
      }
      temp.append("\n");
    }
    $$.code = strdup(temp.c_str());
    $$.place = strdup("");
  }
  | identifiers COLON ENUM L_PAREN identifiers R_PAREN {
    std::string temp;
    // $1.place is a list of variables with possible arrays (x|y|z,i|b,7)
    // $5.place is a list of variables with possible arrays (x|y|z,i|b,7)

    // std::string ident_parse($1.place);
    // std::replace(ident_parse.begin(), ident_parse.end(), '|', ' ');

    std::string type_parse($5.place);
    std::replace(type_parse.begin(), type_parse.end(), '|', ' ');

    // std::stringstream ident_ss(ident_parse);
    std::stringstream type_ss(type_parse);
    // std::string ident_temp;
    std::string type_temp;
    while (type_ss >> type_temp) {
      varTemp[type_temp] = type_temp;
      arrSize[type_temp] = 1;
      temp.append(". " + type_temp + "\n");
      temp.append("= " + type_temp + ", " + new_enum() + "\n");
    }

    // x : enum ( Proc1, Proc2 );
    // y : enum ( Tall, Short );
    // . Proc1
    // = Proc1, 1
    // . Proc2
    // = Proc2, 2
    // . Tall
    // = Tall, 3
    // . Short
    // = Short, 4
    $$.place = strdup("");
    $$.code = strdup(temp.c_str());
  }
  | identifiers COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER { // array 
    size_t left = 0;
    size_t right = 0;
    std::string parse($1.place);
    std::string temp;
    bool flag = false;
    while(!flag) {
      right = parse.find("|", left);
      temp.append(".[] ");
      if (right == std::string::npos) {
        std::string ident = parse.substr(left, right);
        if (reserved.find(ident) != reserved.end()) {
          printf("Error on line %d: Identifier %s's name is a reserved word.\n", currLine, ident.c_str());
          hasErrors = true;
          // exit(1);
        }
        if (funcs.find(ident) != funcs.end() || varTemp.find(ident) != varTemp.end()) {
          printf("Error on line %d: Identifier %s is previously declared.\n", currLine, ident.c_str());
        } else {
          if ($5 <= 0) {
            printf("Declaring array ident %s of size <= 0.\n", ident.c_str());
            exit(0);
          }
          varTemp[ident] = ident;
          arrSize[ident] = $5;
        }
        temp.append(ident + ", " + std::to_string($5) + "\n"); // .[] x, 5 
        flag = true;
      } else {
        std::string ident = parse.substr(left, right-left);
        if (reserved.find(ident) != reserved.end()) {
          printf("Error on line %d: Identifier %s's name is a reserved word.\n", currLine, ident.c_str());
          hasErrors = true;
          // exit(1);
        }
        if (funcs.find(ident) != funcs.end() || varTemp.find(ident) != varTemp.end()) {
          printf("Error line line %d: Identifier %s is previously declared.\n", currLine, ident.c_str());
          hasErrors = true;
          // exit(1);
        } else {
          if ($5 <= 0) {
            printf("Declaring array ident %s of size <= 0.\n", ident.c_str());
            exit(0);
          }
          varTemp[ident] = ident;
          arrSize[ident] = $5;
        }
        temp.append(", ");
        temp.append(std::to_string($5));
        temp.append("\n");
      }
      $$.code = strdup(temp.c_str());
      $$.place = strdup("");
    }
  }
  ;





statements: %empty {
    $$.code = strdup("");
  }
  | statement SEMICOLON statements {
    std::string temp;
    temp.append($1.code);
    temp.append($3.code);
    $$.code = strdup(temp.c_str());
  }

  identifiers: ident { // x|y|a,b|z
    $$.place = $1.place;
    $$.code = strdup("");
  }
  | ident COMMA identifiers {
    std::string temp;
    temp.append($1.place);
    temp.append("|");
    temp.append($3.place);
    $$.place = strdup(temp.c_str());
    $$.code = strdup("");
  };

statement: var ASSIGN expression {
    std::string parse($1.place); // a,i if array 

    if (parse.find(",") != std::string::npos) { // Is an array
      if (varTemp.find(parse.substr(0, parse.find(","))) == varTemp.end()) {
        printf("Error on line %d: using variable %s before defined.\n", currLine, $1.place);
        hasErrors = true;
        // exit(1);
      }
    } else { // Not an array
      if (varTemp.find(parse) == varTemp.end()) {
        printf("Error on line %d: using variable %s before defined.\n",currLine, $1.place);
        hasErrors = true;
        // exit(1);
      }
    }

    std::string temp;
    temp.append($1.code);
    temp.append($3.code);
    std::string middle = $3.place;
    if ($1.arr && $3.arr)
      temp += "[]= ";
    else if ($1.arr)
      temp += "[]= ";
    else if ($3.arr)
      temp += "= ";
    else
      temp += "= ";
    temp.append($1.place); // $1.place = src,index
    temp.append(", ");
    temp.append(middle);
    temp += "\n"; //  []= x,0, 0
    $$.code = strdup(temp.c_str());
  }
	| IF bool_exp THEN statements ENDIF {
    std::string ifS = new_label();
    std::string after = new_label();
    std::string temp;
    temp.append($2.code);
    temp = temp + "?:= " + ifS + ", " + $2.place + "\n"; // True, jump to :ifS code from $4
    temp = temp + ":= " + after + "\n"; // Skip $4 by jumping to l2
    temp = temp + ": " + ifS + "\n";
    temp.append($4.code);
    temp = temp + ": " + after + "\n";
    $$.code = strdup(temp.c_str());
  }
	| IF bool_exp THEN statements ELSE statements ENDIF {
    std::string ifS = new_label();
    std::string after = new_label();
    std::string temp;
    temp.append($2.code);
    temp = temp + "?:= " + ifS + ", " + $2.place + "\n";
    temp.append($6.code);
    temp = temp + ":= " + after + "\n";
    temp = temp + ": " + ifS + "\n";
    temp.append($4.code);
    temp = temp + ": " + after + "\n";
    $$.code = strdup(temp.c_str());
  }
	| WHILE bool_exp BEGINLOOP statements ENDLOOP {
    std::string whileS = new_label();
    std::string label_true = new_label();
    std::string after = new_label();
    std::string parse($4.code);
    std::string temp;
    temp.append(": " + whileS + "\n"); // whileS label
    temp.append($2.code); // evaluate bool_exp
    temp.append("?:= " + label_true + ", " + $2.place + "\n"); // If true, goto label_true
    temp.append(":= " + after + "\n"); // (else) goto after
    temp.append(": " + label_true + "\n"); // label_true
    while(parse.find("continue") != std::string::npos) { // if there is a continue, then replace with a goto whileS
      parse.replace(parse.find("continue"), 8, ":= " + whileS);
    }
    temp.append(parse); // statements
    temp.append(":= " + whileS + "\n"); // go back to start
    temp.append(": " + after + "\n"); // After end of loop
    $$.code = strdup(temp.c_str());
  }
	| DO BEGINLOOP statements ENDLOOP WHILE bool_exp {
    std::string temp;
    std::string beginLoop = new_label();
    std::string beginDoWhile = new_label();

    std::string statement = $3.code;
    std::string dest;
    dest.append(":= ");
    dest.append(beginDoWhile);

    while(statement.find("continue") != std::string::npos) {
      statement.replace(statement.find("continue"), 8, dest);
    }

    temp.append(": ");
    temp.append(beginLoop);
    temp.append("\n");
    temp.append(statement);
    temp.append(": ");
    temp.append(beginDoWhile);
    temp.append("\n");
    temp.append($6.code);
    temp.append("?:= "); // conditional branch
    temp.append(beginLoop);
    temp.append(", ");
    temp.append($6.place);
    temp.append("\n");

    $$.code = strdup(temp.c_str());
  }
	| READ vars {
    std::string temp;
    std::string parse($2.place); // "x y z"
    std::replace(parse.begin(), parse.end(), '|', ' ');
    std::stringstream ss(parse);
    std::string t;
    temp.append($2.code);
    while (ss >> t) {
      if (t.find(",") != std::string::npos) { // If it is an array
        // .[]< src, index

        if (varTemp.find(t.substr(0, t.find(","))) == varTemp.end()) {
          printf("Error on line %d: using variable %s before defined.\n", currLine, t.c_str());
          hasErrors = true;
          // exit(1);
        }
        temp.append(".[]< " + t.substr(0, t.find(",")) + ", " + t.substr(t.find(",")+1) + "\n");
      } else { // If its just a normal variable
        // .< src

        if (varTemp.find(t) == varTemp.end()) {
          printf("Error on line %d: using variable %s before defined.\n", currLine, t.c_str());
          hasErrors = true;
          // exit(1);
        }
        temp.append(".< " + t + "\n");
      }
    }
    $$.code = strdup(temp.c_str());
  }
	| WRITE vars {
    // .> src

    // x = "hello"
    // y = " world"
    // write x, y   => "hello world"

    // 'vars' contains many vars, they may be arrays
    std::string temp;
    std::string parse($2.place); // "x y z"
    std::replace(parse.begin(), parse.end(), '|', ' ');
    std::stringstream ss(parse);
    std::string t;
    temp.append($2.code);
    while (ss >> t) {
      if (t.find(",") != std::string::npos) { // If it is an array
        // .[]> src, index

        if (varTemp.find(t.substr(0, t.find(","))) == varTemp.end()) {
          printf("Error on line %d: Use of undeclared variable %s before defined.\n", currLine, t.c_str());
          hasErrors = true;
          // exit(1);
        }
        temp.append(".[]> " + t.substr(0, t.find(",")) + ", " + t.substr(t.find(",")+1) + "\n");
      } else { // If its just a normal variable
        // .> src
        if (varTemp.find(t) == varTemp.end()) {
          printf("Error on line %d: Use of undeclared variable %s before defined.\n", currLine, t.c_str());
          hasErrors = true;
          // exit(1);
        }
        temp.append(".> " + t + "\n");
      }
    }
    $$.code = strdup(temp.c_str());
  }
	| CONTINUE {
    $$.code = strdup("continue\n");
  }
	| RETURN expression {
    std::string temp;
    temp.append($2.code);
    temp.append("ret " + std::string($2.place) + "\n");
    $$.code = strdup(temp.c_str());
  }
	;

vars: var COMMA vars {
    // .place is list of vars, seperated by |, "newest" variable defined last
    // eg. int x; int y[10]; int z  ==>  "x|y,__t2__|z"
    std::string temp;
    temp.append(std::string($1.place) + "|" + std::string($3.place));
    std::string code;
    code.append($1.code);
    code.append($3.code);
    $$.place = strdup(temp.c_str());
    $$.code = strdup(code.c_str());
  }
  | var {
    $$.place = strdup($1.place);
    $$.code = strdup($1.code);
  }
  ;

bool_exp: relation_and_exp {
    $$.place = $1.place;
    $$.code = $1.code;
  }
  | relation_and_exp OR bool_exp {
    std::string result = new_temp();
    std::string temp;
    temp.append($1.code);
    temp.append($3.code);
    temp.append(". " + result + "\n");
    temp.append("|| " + result + ", " + $1.place + ", " + $3.place + "\n");
    $$.place = strdup(result.c_str());
    $$.code = strdup(temp.c_str());
  }
  ;

relation_and_exp: relation_exp {
    $$.place = $1.place;
    $$.code = $1.code;
  }
  | relation_exp AND relation_and_exp {
    std::string result = new_temp();
    std::string temp;
    temp.append($1.code);
    temp.append($3.code);
    temp.append(". " + result + "\n");
    temp.append("&& " + result + ", " + $1.place + ", " + $3.place + "\n");
    $$.place = strdup(result.c_str());
    $$.code = strdup(temp.c_str());
  }
  ;

relation_exp: NOT expression comp expression {
    std::string result = new_temp();
    std::string temp;
    temp.append($2.code);
    temp.append($4.code);
    temp.append(". " + result + "\n");
    temp.append(std::string($3.place) + " " + result + ", " + $2.place + ", " + $4.place + "\n");
    temp.append("! " + result + ", " + result);
    $$.place = strdup(result.c_str());
    $$.code = strdup(temp.c_str());
  }
  | NOT TRUE {
    $$.place = strdup("0");
    $$.code = strdup("");
    $$.arr = false;
  }
  | NOT FALSE {
    $$.place = strdup("1");
    $$.code = strdup("");
    $$.arr = false;
  }
  | NOT L_PAREN bool_exp R_PAREN {
    std::string inv = new_temp();
    std::string temp;
    temp.append($3.code);
    temp.append(". " + inv + "\n");
    temp.append("! " + inv + ", " + $3.place + "\n");
    $$.place = strdup(inv.c_str());
    $$.code = strdup(temp.c_str());
  }
  | expression comp expression {
    std::string result = new_temp();
    std::string temp;
    temp.append($1.code);
    temp.append($3.code);
    temp.append(". " + result + "\n");
    temp.append(std::string($2.place) + " " + result + ", " + $1.place + ", " + $3.place + "\n");
    $$.place = strdup(result.c_str());
    $$.code = strdup(temp.c_str());
  }
  | TRUE {
    $$.place = strdup("1");
    $$.code = strdup("");
    $$.arr = false;
  }
  | FALSE {
    $$.place = strdup("0");
    $$.code = strdup("");
    $$.arr = false;
  }
  | L_PAREN bool_exp R_PAREN {
    $$.code = $2.code;
    $$.place = $2.place;
  }
  ;


comp: EQ {
    $$.place = strdup("==");
    $$.code = strdup("");
    $$.arr = false;
  }
	| NEQ {
    $$.place = strdup("<>");
    $$.code = strdup("");
    $$.arr = false;
  }
	| LT {
    $$.place = strdup("<");
    $$.code = strdup("");
    $$.arr = false;
  }
	| GT {
    $$.place = strdup(">");
    $$.code = strdup("");
    $$.arr = false;
  }
	| LTE {
    $$.place = strdup("<=");
    $$.code = strdup("");
    $$.arr = false;
  }
	| GTE {
    $$.place = strdup(">=");
    $$.code = strdup("");
    $$.arr = false;
  }
	;

expression: multiplicative_expression {
    $$.place = $1.place;
    $$.code = $1.code;
  }
  | multiplicative_expression ADD expression {
    std::string result = new_temp();
    std::string temp;
    // Evaluate multiplicative_expression
    temp.append($1.code);
    temp.append($3.code);
    temp.append(". " + result + "\n");
    temp.append("+ " + result + ", " + $1.place + ", " + $3.place + "\n");
    $$.place = strdup(result.c_str());
    $$.code = strdup(temp.c_str());
  }
  | multiplicative_expression SUB expression {
    std::string result = new_temp();
    std::string temp;
    // Evaluate multiplicative_expression
    temp.append($1.code);
    temp.append($3.code);
    temp.append(". " + result + "\n");
    temp.append("- " + result + ", " + $1.place + ", " + $3.place + "\n");
    $$.place = strdup(result.c_str());
    $$.code = strdup(temp.c_str());
  }

multiplicative_expression: term {
    $$.place = $1.place;
    $$.code = $1.code;
  }
  | term MULT multiplicative_expression {
    std::string result = new_temp();
    std::string temp;
    // 2 * [some other multiplicative_expression]
    // * dst, src1, src2 	      dst = src1 * src2
    temp.append($1.code);
    temp.append($3.code); // Evaluate other multiplicative expression
    temp.append(". " + result + "\n");
    temp.append("* " + result + ", " + $1.place + ", " + $3.place + "\n");
    $$.place = strdup(result.c_str());
    $$.code = strdup(temp.c_str());
  }
  | term DIV multiplicative_expression {
    std::string result = new_temp();
    std::string temp;
    // 2 * [some other multiplicative_expression]
    // * dst, src1, src2 	      dst = src1 * src2
    temp.append($1.code);
    temp.append($3.code); // Evaluate other multiplicative expression
    temp.append(". " + result + "\n");
    temp.append("/ " + result + ", " + $1.place + ", " + $3.place + "\n");
    $$.place = strdup(result.c_str());
    $$.code = strdup(temp.c_str());
  }
  | term MOD multiplicative_expression {
    std::string result = new_temp();
    std::string temp;
    // 2 * [some other multiplicative_expression]
    // * dst, src1, src2 	      dst = src1 * src2
    temp.append($1.code);
    temp.append($3.code); // Evaluate other multiplicative expression
    temp.append(". " + result + "\n");
    temp.append("% " + result + ", " + $1.place + ", " + $3.place + "\n");
    $$.place = strdup(result.c_str());
    $$.code = strdup(temp.c_str());
  }
  ;

// The final value of 'term' should be placed in .place
term: SUB var {
    std::string parse($2.place);
    if (parse.find(",") != std::string::npos) { // Is an array
      if (varTemp.find(parse.substr(0, parse.find(","))) == varTemp.end()) {
        printf("Error on line %d: using variable %s before defined.\n",currLine, $2.place);
        hasErrors = true;
        // exit(1);
      }
    } else { // Not an array
      if (varTemp.find(parse) == varTemp.end()) {
        printf("Error on line %d: using variable %s before defined.\n", currLine, $2.place);
        hasErrors = true;
        // exit(1);
      }
    }
    std::string neg = new_temp();
    std::string temp;
    temp.append(". " + neg + "\n");
    temp.append("- " + neg + ", 0, " + $2.place + "\n"); // Subtract var from zero -> makes it negative
    $$.place = strdup(std::string(neg).c_str());
    $$.code = strdup(temp.c_str());
  }
  | SUB NUMBER {
    std::string neg = new_temp();
    std::string temp;
    temp.append(". " + neg + "\n");
    temp.append("- " + neg + ", 0, " + std::to_string($2) + "\n");
    $$.place = strdup(neg.c_str());
    $$.code = strdup(temp.c_str());
  }
  | SUB L_PAREN expression R_PAREN {
    std::string neg = new_temp();
    std::string temp;
    temp.append($3.code);
    temp.append(". " + neg + "\n");
    temp.append("- " + neg + ", 0, " + $3.place);
    $$.place = strdup(neg.c_str());
    $$.code = strdup(temp.c_str());
  }
  | var {
    std::string parse($1.place);
    std::string temp;
    std::string holder = new_temp();
    temp.append($1.code);
    temp.append(". " + holder + "\n");
    if (parse.find(",") != std::string::npos) { // Is an array
      if (varTemp.find(parse.substr(0, parse.find(","))) == varTemp.end()) {
        printf("Error on line %d: using variable %s before defined.\n", currLine, $1.place);
        hasErrors = true;
        // exit(1);
      }
      temp.append("=[] " + holder + ", ");
      temp.append(parse.substr(0, parse.find(",")));
      temp.append(parse.substr(parse.find(",")) + "\n");
    } else { // Not an array
      if (varTemp.find(parse) == varTemp.end()) {
        printf("Error on line %d: using variable %s before defined.\n", currLine, $1.place);
        hasErrors = true;
        // exit(1);
      }
      temp.append("= " + holder + ", " + $1.place + "\n");
    }
    $$.place = strdup(holder.c_str());
    $$.code = strdup(temp.c_str());
  }
  | NUMBER {
    $$.place = strdup(std::to_string($1).c_str());
    $$.code = strdup("");
  }
  | L_PAREN expression R_PAREN {
    $$.place = $2.place;
    $$.code = $2.code;
  }
  | ident L_PAREN expressions R_PAREN {
    if(funcs.find(std::string($1.place)) == funcs.end()){
      printf("Error on line %d: Use of undeclared function %s\n", currLine, $1.place);
      hasErrors = true;
      // exit(1);
    }
    // For calling functions -> fib (x-1, x-2)
    //                           ident^    ^exp ^exp
    // 1. Split expressions by "|"
    // 2. For each expression, append "param [expression]"
    // 3. Print "call ident.place, new_temp()"
    std::string temp;
    std::string result = new_temp();

    temp.append($3.code);

    std::string parse($3.place);
    std::replace(parse.begin(), parse.end(), '|', ' ');
    std::stringstream ss(parse);
    std::string t;
    while (ss >> t) {
      temp.append("param " + t + "\n");
    }

    temp.append(". " + result + "\n");
    temp.append("call " + std::string($1.place) + ", " + result + "\n");
    $$.place = strdup(result.c_str());
    $$.code = strdup(temp.c_str());
  }
  ;

expressions: expression {
    $$.place = $1.place;
    $$.code = $1.code;
  }
  | expression COMMA expressions {
    // A | seperated list of expression places
    std::string temp;
    temp.append(std::string($1.place) + "|" + std::string($3.place));
    std::string exprs;
    exprs.append($1.code);
    exprs.append($3.code);
    $$.place = strdup(temp.c_str());
    $$.code = strdup(exprs.c_str());
  }
  ;

// This is the
// used in: statement, vars, term
var: ident {
    // A single variable, used anywhere.
    if (arrSize[$1.place] > 1) {
      printf("Error on line %d: using array variable %s as non-array\n", currLine, $1.place);
      hasErrors = true;
    }

    $$.place = $1.place; // The code will be the identifier itself (eg. 'x') to be used later by whatever uses 'var'
    $$.code = strdup("");
  }
  | ident L_SQUARE_BRACKET expression R_SQUARE_BRACKET {
    if (arrSize[$1.place] == 1) {
      printf("Error on line %d: using non-array variable %s as array\n", currLine, $1.place);
      hasErrors = true;
    }

    std::string temp;
    std::string place;
    // Evaluate expression
    temp.append($3.code);
    // Result is stored in $3.place, so set that as the offset
    place.append($1.place);
    place.append(",");
    place.append($3.place);
    $$.place = strdup(place.c_str()); // "a,i"
    $$.code = strdup(temp.c_str());
    $$.arr = true;
  }
	;

// This is just a name of some variable or function
// .place is the name, .code is empty
ident: IDENT {
    $$.place = strdup($1); // The code will be the identifier itself (eg. 'x') to be used later by whatever uses 'ident'
    $$.code = strdup("");
  }


%%

int current_temp = 0;
std::string new_temp() {
  return "__temp__" + std::to_string(current_temp++);
}

int current_label = 0;
std::string new_label() {
  return "__label__" + std::to_string(current_label++);
}

int current_enum = 0;
std::string new_enum() {
  return std::to_string(current_enum++);
}

int yyerror(const char *s) {
  printf("Line %d, column %d: %s\n", currLine, currPos, s);
  exit(1);
}
