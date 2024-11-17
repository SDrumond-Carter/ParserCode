// Samuel Drumond
// COP3402 Summer 2023
// HW3 - Tiny PL/0 compiler

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define MAX 500 

typedef enum {
oddsym = 1, identsym, numbersym, plussym, minussym,
multsym, slashsym, xorsym, eqlsym, neqsym, lessym, leqsym,
gtrsym, geqsym, lparentsym, rparentsym, commasym, semicolonsym,
periodsym, becomessym, beginsym, endsym, ifsym, thensym,
whilesym, dosym, callsym, constsym, varsym, procsym, writesym,
readsym , elsesym} token_type;

typedef struct
{
int kind; // const = 1, var = 2, proc = 3
char name[10]; // name up to 11 chars
int val; // number (ASCII value)
int level; // L level
int addr; // M address
int mark; // to indicate unavailable or deleted
} symbol;
// symbol_table[MAX];
symbol* table;

//Array holding source program text
char intext[MAX];

// Struct for each lexeme elements
typedef struct lex
{
    int token;
	char *name;
	int value;

} lex;
lex* list;

//Struct to hold the instructions of each element
typedef struct isa
{
	char opcode[4];
	int op;
	int L;
	int M;

} isa;
isa* text;

//variable to keep track of constraints
int comments = 0;
int openVariable = 0;
int openNumber = 0;
int variableLength = 0;
// char *variables[MAX];
char tempVariableName [12];
char digits[5];
int variableAmount = 0;
int numberLength = 0;
// int number[MAX];



int cx = 0;
int tokenIdx = 0;
int loopIdx = 0;
int jpcIdx = 0;
int jmpIdx = 0;
int numVars = 0;
int numCons = 0;
int symIdx = 0;
int endText = 0;
int endOfTable = 0;

// //Array holding reserved and special characters
char *special[] = {"/*", "*/", "<>", "<=", ">=", ":=", "const", "var", "procedure", "call", "begin", "end", "if", "then", "xor", "else", "while",
"do", "read", "write","+", "-" , "*", "/", "(", ")", "=", "," , ".", "<", ">", ";" };

//Implemented functions from the psudo-code 
void emit( char code[], int op, int L, int M);
void addsymb(int kind, char name[], int value, int level, int addr, int mark);
void PROGRAM(lex *list);
int SYMBOLTABLECHECK(lex *list);
void BLOCK(lex *list);
int CONSTDECLARATION(lex *list);
int VARDECLARATION(lex *list);
// int PROCDECLARATION(int *tokens, int lexLevel);
void STATEMENT(lex *list);
void EXPRESSION(lex *list);
void TERM(lex *list);
void FACTOR(lex *list);
void CONDITION(lex *list);

int SYMBOLTABLECHECK(lex *list)
{
	int i = 0;
	while (i < MAX)
	{
		if(strcmp(table[i].name, list[tokenIdx].name) == 0 && list[tokenIdx].token == identsym)
        {
            return i;
        }
		i++;
	}
	return -1;
}

void emit( char code[], int op, int L, int M)
{
    if(cx > MAX)
    {
        printf("Error: index overload");
        exit(0); 
    }
    else
    {
        strcpy(text[cx].opcode, code);
        text[cx].op = op; //opcode
        text[cx].L = L; // lexicographical level
        text[cx].M = M; // modifier
        cx++;
    }
}

void addsymb(int kind, char name[], int value, int level, int addr, int mark)
{
    table[symIdx].kind = kind;
    strcpy(table[symIdx].name, name);
	table[symIdx].val = value;
	table[symIdx].level = level;
	table[symIdx].addr = addr;
	table[symIdx].mark = mark;
	symIdx++;
	endOfTable++;
}

void PROGRAM(lex *list)
{
    BLOCK(list);
    if(list[tokenIdx+1].token != periodsym)
    {
        printf("Error: program must end with period\n");
        exit(0);
    }
    emit("SYM", 9, 0, 3);
}

void BLOCK(lex * list)
{
    CONSTDECLARATION(list);
    numVars = VARDECLARATION(list);
    emit("INC", 6, 0, 3 + numVars);
	STATEMENT(list);
}

int CONSTDECLARATION(lex * list)
{
    char ident[12];
    if(list[tokenIdx].token == constsym)
    {
        do
        {
            numCons++;
            tokenIdx++;
            if(list[tokenIdx].token != identsym)
            {
                printf("Error: const, var, call, and read keywords must be followed by identifier\n");
                exit(0);
            }
            if(SYMBOLTABLECHECK(list) != -1)
            {
                printf("Error: Undeclared Identifier\n");
                exit(0);
            }
            strcpy(ident, list[tokenIdx].name);
            tokenIdx++;
            if(list[tokenIdx].token != eqlsym)
            {
                printf("Error: constants must be assigned with =\n");
                exit(0);
            }
            tokenIdx++;
            if(list[tokenIdx].token != numbersym)
            {
                printf("Error: constants must be assigned an integer value\n");
                exit(0);
            }
            addsymb(1, ident, list[tokenIdx].value, 0, 0, 0);
            tokenIdx++;
        }while(list[tokenIdx].token == commasym);
        if(list[tokenIdx].token != semicolonsym)
        {
            printf("Error: constant and variable declarations must be followed by a semicolon\n");
            exit(0);
        }
        tokenIdx++;
    }
    return numCons;
}

int VARDECLARATION(lex * list)
{
    if(list[tokenIdx].token == varsym)
	{
		do
		{
			numVars++;
			tokenIdx++;
			if(list[tokenIdx].token != identsym)
            {
                printf("Error: const, var, and read keywords must be followed by identifier\n");
                exit(0);
            }
			if(SYMBOLTABLECHECK(list) != -1)
            {
                printf("Error: Undeclared Identifier\n");
                exit(0);
            }
			addsymb(2, list[tokenIdx].name, 0, 0, numVars + 3, 0);
			tokenIdx++;
		} while (list[tokenIdx].token == commasym);
		if(list[tokenIdx].token != semicolonsym)
        {
            printf("Error: constant and variable declarations must be followed by a semicolon\n");
            exit(0);
        }
		tokenIdx++;
	}
	return numVars;
}
void STATEMENT(lex * list)
{
    if(list[tokenIdx].token == identsym)
	{
		symIdx = SYMBOLTABLECHECK(list);
		if(symIdx == -1)
        {
            printf("Error: Undeclared variable\n");
            exit(0);
        }
        if(table[symIdx].kind != 2)
        {
            printf("Error: Undeclared variable\n");
            exit(0);
        }
		tokenIdx++;

		if(list[tokenIdx].token != becomessym)
        {
            printf("Error: assignment statements must use :=\n");
        }
		tokenIdx++;
		EXPRESSION(list);
		emit("STO", 4, table[symIdx].level, table[symIdx].addr);
		return;
	}
    if(list[tokenIdx].token == beginsym)
	{
		do
		{
			tokenIdx++;
			STATEMENT(list);
		} while (list[tokenIdx].token == semicolonsym);
		if(list[tokenIdx].token != endsym)
        {
            printf("Error: begin must be followed by end\n");
            exit(0);
        }
		tokenIdx++;
		return;
	}
    if(list[tokenIdx].token == ifsym)
	{
		tokenIdx++;
		CONDITION(list);
		jpcIdx = cx;
		emit("JPC", 8, 0, 0);
		if(list[tokenIdx].token != thensym)
        {
            printf("Error: if must be followed by then\n");
            exit(0);
        }
		tokenIdx++;
		STATEMENT(list);
		text[jpcIdx].M = cx;
		return;
	}
    if(list[tokenIdx].token == xorsym)
    {
        tokenIdx++;
        CONDITION(list);
        jpcIdx = cx;
		emit("JPC", 8, 0, 0);
        if(list[tokenIdx].token != thensym)
        {
            printf("Error: if must be followed by then\n");
            exit(0);
        }
		tokenIdx++;
		STATEMENT(list);
        if(list[tokenIdx].token != semicolonsym)
        {
            printf("Error: constant and variable declarations must be followed by a semicolon\n");
            exit(0);
        }
        else
        {
            tokenIdx++;
        }
        if(list[tokenIdx].token != elsesym)
        {
            printf("Error: if must be followed by else\n");
            exit(0);
        }
        jmpIdx = cx;
        emit( "JMP", 7, 0, 0);
        tokenIdx++;
        text[jpcIdx].M = cx;
        STATEMENT(list);
        text[jmpIdx].M = cx;
        return;
    }
    if(list[tokenIdx].token == whilesym)
	{
		tokenIdx++;
		loopIdx = cx;
		CONDITION(list);
		if(list[tokenIdx].token != dosym)
        {
            printf("Error: while must be followed by do\n");
            exit(0);
        }
		tokenIdx++;
		jpcIdx = cx;
		emit( "JPC", 8, 0, 0);
		STATEMENT(list);
		emit("JMP", 7, 0, loopIdx);
		text[jpcIdx].M = cx;
		return;
	}
    if(list[tokenIdx].token == readsym)
	{
		tokenIdx++;
		if(list[tokenIdx].token != identsym)
        {
            printf("Error: const, var, and read keywords must be followed by identifier\n");
            exit(0);
        }
		symIdx = SYMBOLTABLECHECK(list);
		if(symIdx == -1)
        {
            printf("Error: Undeclared variable\n");
            exit(0);
        }
        if(table[tokenIdx].kind != 2)
        {
            printf("Error: Undeclared Variable\n");
            exit(0);
        }
        tokenIdx++;
		emit("SYS", 9, 0, 2);
		emit("STO", 4,table[symIdx].level, table[symIdx].addr);
		return;
	}
    if(list[tokenIdx].token == writesym)
	{
		tokenIdx++;
		EXPRESSION(list);
		emit("SYS", 9, 0, 1);
		return;
	}

}

void CONDITION(lex * list)
{
    if (list[tokenIdx].token == oddsym)
	{
		tokenIdx++;
		EXPRESSION(list);
		emit("OPR", 2, 0, 6);
	}
    else
    {
        EXPRESSION(list);
		if (list[tokenIdx].token == eqlsym)
		{
			tokenIdx++;
			EXPRESSION(list);
			emit("OPR", 2, 0, 8);
		}
		else if (list[tokenIdx].token == neqsym)
		{
			tokenIdx++;
			EXPRESSION(list);
			emit("OPR", 2, 0, 9);
		}
		else if (list[tokenIdx].token == lessym)
		{
			tokenIdx++;
			EXPRESSION(list);
			emit("OPR", 2, 0, 10);
		}
		else if (list[tokenIdx].token == leqsym)
		{
			tokenIdx++;
			EXPRESSION(list);
			emit("OPR", 2, 0, 11);
		}
		else if (list[tokenIdx].token == gtrsym)
		{
			tokenIdx++;
			EXPRESSION(list);
			emit("OPR",2 , 0, 12);
		}
		else if (list[tokenIdx].token == geqsym)
		{
			tokenIdx++;
			EXPRESSION(list);
			emit("OPR", 2, 0, 13);
		}
		else
        {
            printf("Error: condition must contain comparison operator\n");
            exit(0);
        }
    }

}

void EXPRESSION(lex * list)
{
    TERM(list);
    while (list[tokenIdx].token == plussym || list[tokenIdx].token == minussym)
    {
        if (list[tokenIdx].token == plussym)
        {
            tokenIdx++;
            TERM(list);
            emit("OPR", 2, 0, 2);
        }
        else
        {
            tokenIdx++;
            TERM(list);
            emit("OPR", 2, 0, 3);
        }
    }
}

void TERM(lex * list)
{
    FACTOR(list);
	while (list[tokenIdx].token == multsym || list[tokenIdx].token == slashsym)
	{
		if (list[tokenIdx].token == multsym)
		{
			tokenIdx++;
			FACTOR(list);
			emit("OPR", 2, 0, 4);
		}
		else
		{
			tokenIdx++;
			FACTOR(list);
			emit("OPR", 2, 0, 5);
		}
	}
}

void FACTOR(lex * list)
{
    if (list[tokenIdx].token == identsym)
	{
		symIdx = SYMBOLTABLECHECK(list);
		if (symIdx == -1)
        {
            printf("Error: symbol name has already been declared\n");
            exit(0);
        }
		if(table[symIdx].kind == 1)
        {
            emit("LIT", 1, 0, table[symIdx].val);
        }
        else
        {
            emit("LOD", 3, table[symIdx].level, table[symIdx].addr);
        }
        tokenIdx++;
	}
	else if (list[tokenIdx].token == numbersym)
	{
		emit("LIT", 1, 0, list[tokenIdx].value);
		tokenIdx++;
	}
	else if (list[tokenIdx].token == lparentsym)
	{
		tokenIdx++;
		EXPRESSION(list);
		if (list[tokenIdx].token != rparentsym)
        {
            printf("Error: right parenthesis must follow left parenthesis\n");
            exit(0);
        }
		tokenIdx++;
	}
	else
    {
        printf("Error: arithmetic equations must contain operands, parentheses, numbers, or symbols\n");
        exit(0);

    }

}
const char * compare(char *special[], char arr[], int a) {
  
  for (int i = 0; i < 32; i++)
  {
    int length = (strlen(special[i]));
    char tmp[length + 1 ];
    memcpy(tmp, arr + a, sizeof(char) * length);

    if(strcmp(tmp, special[i]) == 0)
    {
        return special[i];
    }
  }
  return "null";
}


int main(int argc, char **argv) 
{
    //Allocating memory for all lists and tables
    list = malloc(MAX * sizeof(lex));
    text = malloc(MAX * sizeof(isa));
	table = malloc (MAX * sizeof(symbol));

    // for (int i = 0; i < MAX; i++) {
    // list[i].name = NULL; // Set the name pointer to NULL
    // list[i].value = 0; // Set the value to 0
    // list[i].token = 0; // Set the token to 0
    // }

    // Opening input file as newFile for reading
    char *inf = argv[1];
    FILE *newFile = fopen(inf, "r");
    
    // If the file fail to open, Halt.
    if(newFile == NULL)
    {
        printf("Error: File not found\n");
        return -1;
    }  

    // inserting the values from the file into the intext array
    while (!(feof(newFile))) 
    {
        fscanf(newFile, "%c", &intext[endText]);
        endText++;
    }

    //variables to keep track of the index of the intext and the output arrays
    int listIndex = 0;
    int tableIndex = 0;

    //iterate through the intext array and assign tokes
    for(int i = 0; i <= endText ;i++)
    {
        
        if(strcmp(&intext[i], " ") == 0|| strcmp(&intext[i], "\t") == 0|| strcmp(&intext[i], "\n") == 0)
        {
            continue;
        }


        const char * tmp = compare(special, intext, i);



        //removing comments
        if(strcmp(tmp, "/*") == 0)
        {
            i = i + strlen(tmp) - 1;
            comments = 1;
            continue;
        }
        if(strcmp(tmp, "*/") == 0)
        {
            i = i + strlen(tmp) - 1;
            comments = 0;
            continue;
        }
        if(comments == 1)
        {
            continue;
        }

        //Taking in variables and numbers
        if(strcmp(tmp, "null") == 0)
        {
            if(variableLength == 10)
            {
                printf("Error: variable name is too long\n");
                exit(0);
            }
            if(numberLength == 5)
            {
                printf("Error: number too long");
                exit(0);
            }

            if(isdigit(intext[i]) != 0 && openNumber == 0)
            {
                for(int i = 0; i<numberLength; i++)
                {
                    digits[i] = '\0';
                }
                digits[numberLength] = intext[i];
                numberLength++;
                openNumber = 1;
                continue;
            }
            if(isdigit(intext[i]) != 0 && openNumber == 1)
            {
                digits[numberLength] = intext[i];
                numberLength++;
                continue;
            }
            if(isdigit(intext[i]) == 0 && openNumber == 1)
            {
                if(isalpha(intext[i]) != 0)
                {
                    printf("Error: invalid variable name\n");
                    exit(0);
                }
                list[tableIndex].name = digits;
                list[tableIndex].token = numbersym;
                tableIndex++;
                openNumber = 0;
                numberLength = 0;
                continue;
            }

            if(isalpha(intext[i]) != 0 && openVariable == 0)
            {
                for(int i = 0; i<variableLength; i++)
                {
                    tempVariableName[i] = '\0';
                }
                tempVariableName[variableLength] = intext[i];
                variableLength++;
                openVariable = 1;
                continue;
            }
            if(isalnum(intext[i]) != 0 && openVariable == 1)
            {
                tempVariableName[variableLength] = intext[i];
                variableLength++;
                continue;
            }
            if(isalnum(intext[i]) == 0 && openVariable == 1)
            {
                list[tableIndex].name = tempVariableName;
                list[tableIndex].token = identsym;
                tableIndex++;
                openVariable = 0;
                variableLength = 0;
                variableAmount++;
                continue;
            }
        }

        if(strcmp(tmp, "const") == 0)
        {
            i = i + strlen(tmp) - 1;
            list[tableIndex].name = "const";
            list[tableIndex].token = constsym;
            tableIndex++;
            continue;
        }
        if(strcmp(tmp, "var") == 0)
        {
            i = i + strlen(tmp) - 1;
            list[tableIndex].name = "var";
            list[tableIndex].token = varsym;
            tableIndex++;
            
            continue;
        }
        if(strcmp(tmp, "procedure") == 0)
        {
            i = i + strlen(tmp) - 1;
            list[tableIndex].name = "procedure";
            list[tableIndex].token = procsym;
            tableIndex++;
            continue;
        }
        if(strcmp(tmp, "call") == 0)
        {
            i = i + strlen(tmp) - 1;
            list[tableIndex].name = "call";
            list[tableIndex].token = callsym;
            tableIndex++;
            continue;
        }
        if(strcmp(tmp, "begin") == 0)
        {
            i = i + strlen(tmp) - 1;
            list[tableIndex].name = "begin";
            list[tableIndex].token = beginsym;
            tableIndex++;
            continue;
        }
        if(strcmp(tmp, "end") == 0)
        {
            i = i + strlen(tmp) - 1;
            list[tableIndex].name = "end";
            list[tableIndex].token = endsym;
            tableIndex++;
            continue;
        }
        if(strcmp(tmp, "if") == 0)
        {
            i = i + strlen(tmp) - 1;
            list[tableIndex].name = "if";
            list[tableIndex].token = ifsym;
            tableIndex++;
            continue;
        }
        if(strcmp(tmp, "then") == 0)
        {
            i = i + strlen(tmp) - 1;
            list[tableIndex].name = "then";
            list[tableIndex].token = thensym;
            tableIndex++;
            continue;
        }
        if(strcmp(tmp, "xor") == 0)
        {
            i = i + strlen(tmp) - 1;
            list[tableIndex].name = "xor";
            list[tableIndex].token = xorsym;
            tableIndex++;
            continue;
        }
        if(strcmp(tmp, "else") == 0)
        {
            i = i + strlen(tmp) - 1;
            list[tableIndex].name = "else";
            list[tableIndex].token = elsesym;
            tableIndex++;
            continue;
        }
        if(strcmp(tmp, "while") == 0)
        {
            i = i + strlen(tmp) - 1;
            list[tableIndex].name = "while";
            list[tableIndex].token = whilesym;
            tableIndex++;
            continue;
        }
        if(strcmp(tmp, "do") == 0)
        {
            i = i + strlen(tmp) - 1;
            list[tableIndex].name = "do";
            list[tableIndex].token = dosym;
            tableIndex++;
            continue;
        }
        if(strcmp(tmp, "read") == 0)
        {
            i = i + strlen(tmp) - 1;
            list[tableIndex].name = "read";
            list[tableIndex].token = readsym;
            tableIndex++;
            continue;
        }
        if(strcmp(tmp, "write") == 0)
        {
            i = i + strlen(tmp) - 1;
            list[tableIndex].name = "write";
            list[tableIndex].token = writesym;
            tableIndex++;
            continue;
        }
        if(strcmp(tmp, "+") == 0)
        {
            i = i + strlen(tmp) - 1;
            list[tableIndex].name = "+";
            list[tableIndex].token = plussym;
            tableIndex++;
            continue;
        }
        if(strcmp(tmp, "-") == 0)
        {
            i = i + strlen(tmp) - 1;
            list[tableIndex].name = "-";
            list[tableIndex].token = minussym;
            tableIndex++;
            continue;
        }
        if(strcmp(tmp, "*") == 0)
        {
            i = i + strlen(tmp) - 1;
            list[tableIndex].name = "*";
            list[tableIndex].token = multsym;
            tableIndex++;
            continue;
        }
        if(strcmp(tmp, "/") == 0)
        {
            i = i + strlen(tmp) - 1;
            list[tableIndex].name = "/";
            list[tableIndex].token = slashsym;
            tableIndex++;
            continue;
        }
        if(strcmp(tmp, "(") == 0)
        {
            i = i + strlen(tmp) - 1;
            list[tableIndex].name = "(";
            list[tableIndex].token = lparentsym;
            tableIndex++;
            continue;
        }
        if(strcmp(tmp, ")") == 0)
        {
            i = i + strlen(tmp) - 1;
            list[tableIndex].name = ")";
            list[tableIndex].token = rparentsym;
            tableIndex++;
            continue;
        }
        if(strcmp(tmp, "=") == 0)
        {
            i = i + strlen(tmp) - 1;
            list[tableIndex].name = "=";
            list[tableIndex].token = eqlsym;
            tableIndex++;
            continue;
        }
        if(strcmp(tmp, ",") == 0)
        {
            i = i + strlen(tmp) - 1;
            list[tableIndex].name = ",";
            list[tableIndex].token = commasym;
            tableIndex++;
            continue;
        }
        if(strcmp(tmp, ".") == 0)
        {
            i = i + strlen(tmp) - 1;
            list[tableIndex].name = ".";
            list[tableIndex].token = periodsym;
            tableIndex++;
            continue;
        }
        if(strcmp(tmp, "<") == 0)
        {
            i = i + strlen(tmp) - 1;
            list[tableIndex].name = "<";
            list[tableIndex].token = lessym;
            tableIndex++;
            continue;
        }
        if(strcmp(tmp, "<=") == 0)
        {
            i = i + strlen(tmp) - 1;
            list[tableIndex].name = "<=";
            list[tableIndex].token = leqsym;
            tableIndex++;
            continue;
        }
        if(strcmp(tmp, ">") == 0)
        {
            i = i + strlen(tmp) - 1;
            list[tableIndex].name = ">";
            list[tableIndex].token = gtrsym;
            tableIndex++;
            continue;
        }
        if(strcmp(tmp, ">=") == 0)
        {
            i = i + strlen(tmp) - 1;
            list[tableIndex].name = ">=";
            list[tableIndex].token = geqsym;
            tableIndex++;
            continue;
        }
        if(strcmp(tmp, "<>") == 0)
        {
            i = i + strlen(tmp) - 1;
            list[tableIndex].name = "<>";
            list[tableIndex].token = neqsym;
            tableIndex++;
            continue;
        }
        
        if(strcmp(tmp, ";") == 0)
        {
            i = i + strlen(tmp) - 1;
            list[tableIndex].name = ";";
            list[tableIndex].token = semicolonsym;
            tableIndex++;
            continue;
        }
        if(strcmp(tmp, ":=") == 0)
        {
            i = i + strlen(tmp) - 1;
            list[tableIndex].name = ":=";
            list[tableIndex].token = becomessym;
            tableIndex++;
            continue;
        }
    }

    
    PROGRAM(list);

    //Printing the output to the screen
    int i = 0;
	printf("Generated Assembly:\n");
	printf("Line    OP    L    M\n");
	while (i < cx)
	{
		printf("%3d    %s%5d%5d\n", i, text[i].opcode, text[i].L, text[i].M);
		i++;
	}
	printf("\n");
	return 0;
}