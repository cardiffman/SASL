#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <string>
#include <iostream>
#include <fstream>
#include <map>
#include <list>
#include <stack>
#include <vector>
#include <memory>

using namespace std;

/*
 * 10/1/19
 * Conversion to C++
 * Conversion of list_t to Node class heirarchy,
 * with visitor pattern for implementing certain things.
 */
/*
 * 7/5/05
 * Fixed memory leak (when using GC) in strdup.
 * Removed useless atom table.
 * Added lots of comments.
 * Reformatting.
 * Dead code elimination.
 * Factored cascaded conditionals and replaced some with switch.
 * list_eq for pairs fixed.
 * Fixed parsing of binary operators.
 */


int current_char;

int next_char() {
	return current_char = cin.get();
}

void error_msg(const char *a) {
    /* Potential buffer overflow */
	string line;std::getline(cin,line);
    cerr << "Error: " << a << "\nBefore: " << line << "\n";
    exit(1);
}

struct Comb;
//struct Atom;
struct SComb;
struct KComb;
struct IComb;
struct YComb;
struct UComb;
struct Int;
struct Pair;
struct Variable;
struct Cond;
struct PlusComb;
struct MinusComb;
struct EqualComb;
struct PairComb;
struct LessComb;
struct TimesComb;
struct HeadComb;
struct TailComb;
struct TrueAtom;
struct FalseAtom;
struct NilAtom;
struct DivideComb;
struct LessEqComb;
struct OrComb;
struct AndComb;
struct NotComb;
struct BComb;
struct CComb;
struct SDashComb;
struct BStarComb;
struct CDashComb;
struct CombVisitor {
	virtual ~CombVisitor() {}
	virtual void visitComb(Comb* c)=0;
	//virtual void visitAtom(Atom* c)=0;
	virtual void visitSComb(SComb* c)=0;
	virtual void visitKComb(KComb* c)=0;
	virtual void visitIComb(IComb* c)=0;
	virtual void visitYComb(YComb* c)=0;
	virtual void visitUComb(UComb* c)=0;
	virtual void visitPlusComb(PlusComb* c)=0;
	virtual void visitMinusComb(MinusComb* c)=0;
	virtual void visitEqualComb(EqualComb* c)=0;
	virtual void visitPairComb(PairComb* c)=0;
	virtual void visitLessComb(LessComb* c)=0;
	virtual void visitTimesComb(TimesComb* c)=0;
	virtual void visitHeadComb(HeadComb* c)=0;
	virtual void visitTailComb(TailComb* c)=0;
	virtual void visitTrueAtom(TrueAtom* c)=0;
	virtual void visitFalseAtom(FalseAtom* c)=0;
	virtual void visitNilAtom(NilAtom* c)=0;
	virtual void visitDivideComb(DivideComb* c)=0;
	virtual void visitLessEqComb(LessEqComb* c)=0;
	virtual void visitOrComb(OrComb* c)=0;
	virtual void visitAndComb(AndComb* c)=0;
	virtual void visitNotComb(NotComb* c)=0;
	virtual void visitBComb(BComb* c)=0;
	virtual void visitCComb(CComb* c)=0;
	virtual void visitSDashComb(SDashComb* c)=0;
	virtual void visitBStarComb(BStarComb* c)=0;
	virtual void visitCDashComb(CDashComb* c)=0;
	virtual void visitInt(Int* c)=0;
	virtual void visitPair(Pair* c)=0;
	virtual void visitVariable(Variable* c)=0;
	virtual void visitCond(Cond* c)=0;
};
struct Node {
	virtual ~Node() {}
	Node() :head(nullptr),tail(nullptr) {}
	Node(Node* head, Node* tail) : head(head),tail(tail) {}
	virtual bool is_atom() const { return false; }
	virtual void visit(CombVisitor* v)=0;
	virtual string to_string() const = 0;
	Node* head;
	Node* tail;
};
struct Comb : public Node {
	Comb() {}
	//Comb(Node* head, Node* tail) : head(head),tail(tail){}
	Comb(Node* head, Node* tail) : Node(head, tail) {}
	virtual bool is_atom() const { return false; }
	virtual void visit(CombVisitor* v) { v->visitComb(this); };
	string to_string() const;
};
string Comb::to_string() const {
	return (head?head->to_string():"NULL")+"(" + (tail?tail->to_string():"NULL") + ")";
}
struct SComb : public Comb {
	SComb() {}
	virtual bool is_atom() const { return true; }
	virtual void visit(CombVisitor* v) { v->visitSComb(this); }
	string to_string() const { return "S"; }
};
struct KComb : public Comb {
	KComb() {}
	virtual bool is_atom() const { return true; }
	virtual void visit(CombVisitor* v) { v->visitKComb(this); }
	string to_string() const { return "K"; }
};
struct IComb : public Comb {
	IComb() {}
	virtual bool is_atom() const { return true; }
	virtual void visit(CombVisitor* v) { v->visitIComb(this); }
	string to_string() const { return "I"; }
};
struct YComb : public Comb {
	YComb() {}
	virtual bool is_atom() const { return true; }
	virtual void visit(CombVisitor* v) { v->visitYComb(this); }
	string to_string() const { return "Y"; }
};
struct UComb : public Comb {
	UComb() {}
	virtual bool is_atom() const { return true; }
	virtual void visit(CombVisitor* v) { v->visitUComb(this); }
	string to_string() const { return "U"; }
};
struct PlusComb : public Comb {
	PlusComb() {}
	virtual bool is_atom() const { return true; }
	virtual void visit(CombVisitor* v) { v->visitPlusComb(this); }
	string to_string() const { return "+"; }
};
struct MinusComb : public Comb {
	MinusComb() {}
	virtual bool is_atom() const { return true; }
	virtual void visit(CombVisitor* v) { v->visitMinusComb(this); }
	string to_string() const { return "-"; }
};
struct EqualComb : public Comb {
	EqualComb() {}
	virtual bool is_atom() const { return true; }
	virtual void visit(CombVisitor* v) { v->visitEqualComb(this); }
	string to_string() const { return "=="; }
};
struct PairComb : public Comb {
	PairComb() {}
	virtual bool is_atom() const { return true; }
	virtual void visit(CombVisitor* v) { v->visitPairComb(this); }
	string to_string() const { return "PP"; }
};
struct LessComb : public Comb {
	LessComb() {}
	virtual bool is_atom() const { return true; }
	virtual void visit(CombVisitor* v) { v->visitLessComb(this); }
	string to_string() const { return "<"; }
};
struct TimesComb : public Comb {
	TimesComb() {}
	virtual bool is_atom() const { return true; }
	virtual void visit(CombVisitor* v) { v->visitTimesComb(this); }
	string to_string() const { return "*"; }
};
struct HeadComb : public Comb {
	HeadComb() {}
	virtual bool is_atom() const { return true; }
	virtual void visit(CombVisitor* v) { v->visitHeadComb(this); }
	string to_string() const { return "HD"; }
};
struct TailComb : public Comb {
	TailComb() {}
	virtual bool is_atom() const { return true; }
	virtual void visit(CombVisitor* v) { v->visitTailComb(this); }
	string to_string() const { return "TL"; }
};
struct TrueAtom : public Node {
	TrueAtom() {}
	virtual bool is_atom() const { return true; }
	virtual void visit(CombVisitor* v) { v->visitTrueAtom(this); }
	string to_string() const { return "T"; }
};
struct FalseAtom : public Node {
	FalseAtom() {}
	virtual bool is_atom() const { return true; }
	virtual void visit(CombVisitor* v) { v->visitFalseAtom(this); }
	string to_string() const { return "F"; }
};
struct NilAtom : public Node {
	NilAtom() {}
	virtual bool is_atom() const { return true; }
	virtual void visit(CombVisitor* v) { v->visitNilAtom(this); }
	string to_string() const { return "NIL"; }
};
struct DivideComb : public Comb {
	DivideComb() {}
	virtual bool is_atom() const { return true; }
	virtual void visit(CombVisitor* v) { v->visitDivideComb(this); }
	string to_string() const { return "/"; }
};
struct LessEqComb : public Comb {
	LessEqComb() {}
	virtual bool is_atom() const { return true; }
	virtual void visit(CombVisitor* v) { v->visitLessEqComb(this); }
	string to_string() const { return "<="; }
};
struct OrComb : public Comb {
	OrComb() {}
	virtual bool is_atom() const { return true; }
	virtual void visit(CombVisitor* v) { v->visitOrComb(this); }
	string to_string() const { return "|"; }
};
struct AndComb : public Comb {
	AndComb() {}
	virtual bool is_atom() const { return true; }
	virtual void visit(CombVisitor* v) { v->visitAndComb(this); }
	string to_string() const { return "&"; }
};
struct NotComb : public Comb {
	NotComb() {}
	virtual bool is_atom() const { return true; }
	virtual void visit(CombVisitor* v) { v->visitNotComb(this); }
	string to_string() const { return "!"; }
};
struct BComb : public Comb {
	BComb() {}
	virtual bool is_atom() const { return true; }
	virtual void visit(CombVisitor* v) { v->visitBComb(this); }
	string to_string() const { return "B"; }
};
struct CComb : public Comb {
	CComb() {}
	virtual bool is_atom() const { return true; }
	virtual void visit(CombVisitor* v) { v->visitCComb(this); }
	string to_string() const { return "C"; }
};
struct SDashComb : public Comb {
	SDashComb() {}
	virtual bool is_atom() const { return true; }
	virtual void visit(CombVisitor* v) { v->visitSDashComb(this); }
	string to_string() const { return "S-"; }
};
struct BStarComb : public Comb {
	BStarComb() {}
	virtual bool is_atom() const { return true; }
	virtual void visit(CombVisitor* v) { v->visitBStarComb(this); }
	string to_string() const { return "B*"; }
};
struct CDashComb : public Comb {
	CDashComb() {}
	virtual bool is_atom() const { return true; }
	virtual void visit(CombVisitor* v) { v->visitCDashComb(this); }
	string to_string() const { return "C-"; }
};
struct Int : public Node {
	Int(int v) : v(v) {}
	void visit(CombVisitor* v) { v->visitInt(this); }
	string to_string() const { return "Int("+std::to_string(v)+")"; }
	int v;
};
struct Pair : public Node {
	Pair(Node* head_, Node* tail_) : Node(head_,tail_){}
	virtual void visit(CombVisitor* v) { v->visitPair(this); }
	string to_string() const;
};
struct Cond : public Comb {
	Cond() : Comb() {}
	void visit(CombVisitor* v) { v->visitCond(this); }
	string to_string() const { return "if"; }
	bool is_atom() const { return true; }
};
/*
 * Type Pair is a node that represents a pair of things.
 * Contrast PairComb which is a combinator that makes pairs of things.
 */
string Pair::to_string() const {
	string out = "[" + head->to_string();
	Node* n = tail;
	Pair* np = dynamic_cast<Pair*>(n);
	while (np) {
		out += ',';
		out += n->to_string();
		n = np->tail;
		np = dynamic_cast<Pair*>(n);
	}
	out += "]";
	return out;
}
struct Variable : public Node {
	Variable(const string& a) : var_name(a) {} // what goes here?
	virtual void visit(CombVisitor* v) { v->visitVariable(this); }
	string var_name;
	string to_string() const { return "Var("+var_name+")"; }
};

/*
 * Convenience macros for list access.
 */
#define H(a) ((a)->head)
#define T(a) ((a)->tail)
#define TT(a) T(T(a))
#define TH(a) T(H(a))
#define HT(a) H(T(a))
#define HH(a) H(H(a))
#define TTH(a) T(TH(a))
#define HTH(a) H(TH(a))
#define THT(a) T(HT(a))
#define HHT(a) H(HT(a))
#define THTH(a) T(HTH(a))
#define HHTH(a) H(HTH(a))

int is_comb(Node * c);

Comb *make_list() {
    Comb *c = new Comb();

    return c;
}

IComb *I;

/*
 * Remove redundant I combinator.
 */
Node *elide(Node *a) {
	//auto a = dynamic_cast<Nod*>(n);
	//if (!a)
	//	return n;
    return H(a)==I ? elide(T(a)) : a;
}

/*
 * Apply combinator a to b.
 * Ie. a b
 */
Comb *apply(Node *a,Node *b) {
    Comb *c = make_list();
    //c->type = COMB_COMB;
    c->head = elide(a);
    c->tail = b; /* elide(b) */;

    return c;
}

/*
 * a b c
 */
Comb *apply2(Node *a,Node *b,Node *c) {
    return apply(apply(a,b),c);
}

/*
 * a b c d
 */
Comb *apply3(Node *a,Node *b,Node *c,Node *d) {
    return apply(apply(apply(a,b),c),d);
}

Variable *make_var(const string& a) {
    auto l = new Variable(a);

    return l;
}

Int *make_int(int a) {
    auto l = new Int(a);

    return l;
}

namespace sapl {
Pair *make_pair(Node *a,Node *b) {
    auto l = new Pair(a, b);

    return l;
}
}

int is_atom(Node *a) {
    return a->is_atom();
}

int is_int(Node *a) {
    return dynamic_cast<Int*>(a)!=nullptr;
}

int is_var(Node *a) {
    return dynamic_cast<Variable*>(a)!=nullptr;
}

int is_comb(Node *a) {
    return dynamic_cast<Comb*>(a)!=nullptr;
}

int is_pair(Node *a) {
    return dynamic_cast<Pair*>(a)!=nullptr;
}

int get_int(Node *a) {
	auto i = dynamic_cast<Int*>(a);
    if (i == nullptr) {
	error_msg("Attempt to read non-int as int");
    }
    return i->v;
}

/*
 * Built in combinators.
 */
SComb *S;
KComb *K;//,*I;
namespace sapl {
PlusComb *plus;
MinusComb *minus;
EqualComb *equal;
PairComb *pair;
LessComb *less;
TimesComb *times;
}
HeadComb *head;
TailComb *tail;
TrueAtom *truec;
FalseAtom *falsec;
Cond *cond = new Cond();
UComb *U; YComb *Y;
NilAtom *nil;
DivideComb *divide;
LessEqComb *lesseq;
OrComb *cor;
AndComb *cand;
NotComb *cnot;
BComb *B;
CComb *C;
SDashComb *Sdash;
BStarComb *Bstar;
CDashComb *Cdash;


typedef map<string,Node*> dictionary;

Node** lookup(dictionary& pointer, const string& name) {
	auto p = pointer.find(name);
	if (p == pointer.end()) {
		pointer[name] = nullptr;
		return &pointer[name];
	}
	return &p->second;
}

/*
 * Test whether dictionary contains given string
 * as key.
 */
int contains(const dictionary& pointer,const string& name) {
	return pointer.find(name) != pointer.end();
}

/*
 * Lexer
 */
enum token_type {
    TOKEN_ERROR,

    TOKEN_IDENTIFIER,
    TOKEN_CONSTANT,

    TOKEN_LPAREN, TOKEN_RPAREN,
    TOKEN_NIL,
    TOKEN_EQUAL,

    /* Definitions */
    TOKEN_DEF, TOKEN_WHERE, TOKEN_SEMICOLON, TOKEN_PERIOD,

    /* List operations */
    TOKEN_COLON, TOKEN_HEAD, TOKEN_TAIL,

    /* Conditional */
    TOKEN_IF, TOKEN_THEN, TOKEN_ELSE,

    /* Arithmetic */
    TOKEN_PLUS, TOKEN_MINUS, TOKEN_TIMES, TOKEN_DIVIDE,

    /* Comparison */
    TOKEN_GREATER, TOKEN_LESS, TOKEN_GREATEREQ, TOKEN_LESSEQ,

    /* Logical */
    TOKEN_OR,TOKEN_AND,TOKEN_NOT, TOKEN_TRUE, TOKEN_FALSE, TOKEN_EOF
};

struct token {
    token_type type;
    string string_value;
    union {
        token_type *token_value;
        int int_value;
    } value;
    token(token_type type, const string& string_value)
    : type(type)
    , string_value(string_value)
    {

    }
    token(token_type type, token_type* tv)
    : type(type)
    {
        value.token_value=(tv);

    }
    token(token_type type, int tv)
    : type(type)
    {
        value.int_value=(tv);

    }
};

token *current_token;

token *make_token_string(token_type t, const string& v) {
	token* tok = new token(t, v);
    return tok;
}

token *make_token_token(token_type t,token_type *v) {
	return new token(t, v);
}

token *make_token_int(token_type t,int v) {
	return new token(t, v);
}

dictionary keywords;

/*
 * Parse either a keyword or an identifier.
 * First character should be alphabetic.
 */
token *lex_keyword_or_identifier() {
    /* Potential buffer overflow */
    string name;
    //int i = 0;

    name.append(1, current_char);
    next_char();

    while (isalnum(current_char)) {
        name.append(1, current_char);
		next_char();
    }

    if (contains(keywords,name)) {
	return make_token_token(*(token_type *)lookup(keywords,name),0);
    }

    return make_token_string(TOKEN_IDENTIFIER,name);
}

/*
 * Parse integer. First character should be
 * digit.
 */
int get_number() {
	int int_value = current_char - '0';
	next_char();
	while (isdigit(current_char)) {
		int_value = int_value*10 + current_char-'0';
		next_char();
	}
	return int_value;
}

token *lex_numeric_constant() {
    return make_token_int(TOKEN_CONSTANT,get_number());
}

/*
 * Parse various types of operator.
 */
token *lex_operator() {
	static const char *op_chars = "()=.:;+-*/";
	const char *p = index(op_chars, current_char);
	if (p != NULL) {
		static token_type token_table[] = { TOKEN_LPAREN, TOKEN_RPAREN, TOKEN_EQUAL,
				TOKEN_PERIOD, TOKEN_COLON, TOKEN_SEMICOLON, TOKEN_PLUS,
				TOKEN_MINUS, TOKEN_TIMES, TOKEN_DIVIDE };
		next_char();
		return make_token_int(token_table[p - op_chars], 0);
	}

	if (current_char == '<') {
		if (next_char() == '=') {
			next_char();
			return make_token_int(TOKEN_LESSEQ, 0);
		}
		return make_token_int(TOKEN_LESS, 0);
	}
	if (current_char == '>') {
		if (next_char() == '=') {
			next_char();
			return make_token_int(TOKEN_GREATEREQ, 0);
		}
		return make_token_int(TOKEN_GREATER, 0);
	}
	error_msg("Unrecognised symbol");
	return nullptr;
}

void lex_white_space() {
    while (isspace(current_char)) {
	next_char();
    }
}

/*
 * Return next token in input.
 * Returns a keyword, identifier, number, operator
 * or end-of-file marker.
 */
token *lex() {
    lex_white_space();
    if (isalpha(current_char)) {
	return lex_keyword_or_identifier();
    } else if (isdigit(current_char)) {
	return lex_numeric_constant();
    } else if (current_char==EOF) {
	return make_token_int(TOKEN_EOF,0);
    } else {
	return lex_operator();
    }
}

/*
 * The parser code follows.
 * This is a simple recursive descent parser.
 * SASL doesn't need anything fancy.
 * Note that the grammar has been refactored slightly
 * from the BNF in the comments.
 */

Node *parse_expr();

/*
 * Expect a specified single token in the parse stream.
 * E.g. after an 'if' we expect a 'then' and an 'else'
 */
void expect(token_type type,const char *message) {
    if (current_token->type!=type) {
	error_msg(message);
    }
    current_token = lex();
}

/*
 * CONDEXPR := 'if' EXPR 'then' EXPR 'else' EXPR
 */
Node *parse_condexpr() {
    Node *a,*b,*c;
    expect(TOKEN_IF,"Expected 'if'");
    a = parse_expr();
    expect(TOKEN_THEN,"Missing 'then'");
    b = parse_expr();
    expect(TOKEN_ELSE,"Missing 'else'");
    c = parse_expr();

    return apply3(cond,a,b,c);
}

/*
 * NAME := identifier
 */
Variable *parse_name() {
	if (current_token->type == TOKEN_IDENTIFIER) {
		Variable *r = make_var(current_token->string_value);
		current_token = lex();

		return r;
	} else {
		error_msg("Expected a name"); return nullptr;
	}
}

/*
 * ATOMIC := CONSTANT | IDENTIFIER | '(' EXPR ')' | 'true' | 'false' | 'nil' |
 *		'head' | 'tail' | 'not'
 */
Node *parse_atomic() {
	Node *r;
	switch (current_token->type) {
	case TOKEN_CONSTANT:
		r = make_int(current_token->value.int_value);
		current_token = lex();
		return r;
	case TOKEN_IDENTIFIER:
		r = make_var(current_token->string_value);
		current_token = lex();
		return r;
	case TOKEN_LPAREN:
		current_token = lex();
		r = parse_expr();
		expect(TOKEN_RPAREN, "Missing ')'");
		return r;
	case TOKEN_TRUE:
		current_token = lex();
		return truec;
	case TOKEN_FALSE:
		current_token = lex();
		return falsec;
	case TOKEN_NIL:
		current_token = lex();
		return nil;
	case TOKEN_HEAD:
		current_token = lex();
		return head;
	case TOKEN_TAIL:
		current_token = lex();
		return tail;
	case TOKEN_NOT:
		current_token = lex();
		return cnot;
	default:
		error_msg("Parse error"); return nullptr;
	}
}

Node *parse_sequence(Node *r,token_type token,Node *(*type)(),Node *op) {
	while (current_token->type == token) {
		current_token = lex();
		r = apply2(op, r, (*type)());
	}
	return r;
}

Node *parse_rsequence(Node *r,token_type token,Node *(*type)(),Node *op) {
	while (current_token->type == token) {
		current_token = lex();
		r = apply2(op, (*type)(), r);
	}
	return r;
}

/*
 * PRODUCT := ATOMIC { ('*' | '/') ATOMIC }
 */
Node *parse_product() {
	Node *r = parse_atomic(), *cor;
	do {
		cor = r;
		r = parse_sequence(r, TOKEN_TIMES, parse_atomic, sapl::times);
		r = parse_sequence(r, TOKEN_DIVIDE, parse_atomic, divide);
	} while (r != cor);
	return r;
}

/*
 * SUM := PRODUCT { ('+' | '-') PRODUCT }
 */
Node *parse_sum() {
    Node *r = parse_product(),*cor;
    do {
	cor = r;
	r = parse_sequence(r,TOKEN_PLUS,parse_product,sapl::plus);
	r = parse_sequence(r,TOKEN_MINUS,parse_product,sapl::minus);
    } while (r!=cor);
    return r;
}

/*
 * COMPARISONEXPR := SUM { ('=' | '<' | '>' | '<=' | '>=') SUM }
 */
Node *parse_comparisonexpr() {
    Node *r = parse_sum(),*cor;
    do {
	cor = r;
	r = parse_sequence(r,TOKEN_EQUAL,parse_sum,sapl::equal);
	r = parse_sequence(r,TOKEN_LESS,parse_sum,sapl::less);
	r = parse_sequence(r,TOKEN_LESSEQ,parse_sum,lesseq);
	r = parse_rsequence(r,TOKEN_GREATER,parse_sum,sapl::less);
	r = parse_rsequence(r,TOKEN_GREATEREQ,parse_sum,lesseq);
    } while (r!=cor);
    return r;
}

/*
 * LOGICALPRODUCT := COMPARISONEXPR { 'and' COMPARISONEXPR }
 */
Node *parse_logicalproduct() {
    Node *r = parse_comparisonexpr();
    r = parse_sequence(r,TOKEN_AND,parse_comparisonexpr,cand);
    return r;
}

/*
 * LOGICALSUM := LOGICALPRODUCT { 'or' LOGICALPRODUCT }
 */
Node *parse_logicalsum() {
    Node *r = parse_logicalproduct();
    r = parse_sequence(r,TOKEN_OR,parse_logicalproduct,cor);
    return r;
}

/*
 * COMBEXPR := LOGICALSUM { LOGICALSUM }
 */
Node *parse_combexpr() {
	Node *r = parse_logicalsum();
	for (;;) {
		/*
		 * Identify end of sequence of arguments to
		 * combinator.
		 */
		static char final[] = { TOKEN_EOF, TOKEN_RPAREN, TOKEN_THEN, TOKEN_ELSE,
				TOKEN_PERIOD, TOKEN_SEMICOLON, TOKEN_WHERE, TOKEN_COLON, 0 };
		if (index(final, current_token->type)) {
			return r;
		}
		r = apply(r, parse_atomic());
	}
	return nullptr;
}

/*
 * LISTEXPR := COMBEXPR [ ':' LISTEXPR ]
 */
Node *parse_listexpr() {
    Node *r = parse_combexpr();
    r = parse_sequence(r,TOKEN_COLON,parse_listexpr,sapl::pair);
    return r;
}

Node *parse_abstraction();
Comb *parse_recursive_abstraction(Comb *name);
Node *abstract(Variable *var,Node *expr);
int mutual_recursion(list<pair<Variable*,Node*>> const & l);

void display(Node *);

/*
 * This is the main expression parser.
 * This is also where recursive defintions (as well as mutually
 * recursive definitions) are identified.
 *
 * EXPR := (CONDEXPR | LISTEXPR)
 *	{ WHERE NAME ABSTRACTION ';' { NAME ABSTRACTION ';' } }
 */
Node *parse_expr() {
    Node *r;//,*abstraction;
    if (current_token->type==TOKEN_IF) {
	    return parse_condexpr();
    }
    r = parse_listexpr();
    for (;;) {
	if (current_token->type==TOKEN_WHERE) {
	    list<pair<Variable*,Node*>> definitions;
	    Variable *name; Node *lhs,*rhs;

	    /*
	     * Get list of all definitions in this 'where' clause.
	     */
		do {
			Node *expr;
			current_token = lex();
			name = parse_name();
			expr = parse_abstraction();
			definitions.push_back(make_pair(name,expr));
		} while (current_token->type == TOKEN_SEMICOLON);

		rhs = nil;
		lhs = apply(K, r);
		if (!mutual_recursion(definitions)) {
			for (auto def: definitions) {
				rhs = apply(apply(sapl::pair, def.second), rhs);
				lhs = apply(U, abstract(def.first, lhs));
			}
			return apply(lhs, rhs);
		} else {
			/*
			 * Mutually recursive definitions.
			 */
			for (auto def : definitions) {
				rhs = apply(apply(sapl::pair, def.second), rhs);
				lhs = apply(U, abstract(def.first, lhs));
			}
			rhs = apply(K, rhs);
			for (auto def : definitions) {
				rhs = apply(U, abstract(def.first, rhs));
			}
			rhs = apply(Y, rhs);
			return apply(lhs, rhs);
		}

#if 0
	    /*
	     * Name of variable being defined.
	     * Not sure why I commented this out.
	     */
	    name = parse_name();
	    abstraction = parse_recursive_abstraction(name);
	    r = abstract(name,r);
	    return apply(r,abstraction);
#endif
		} else {
			return r;
		}
    }
    return r;
}

string get_var(Node *a) {
    auto v = dynamic_cast<Variable*>(a);
    if (!v)
    	error_msg("Attempt to read non-var as var");
    return v->var_name;
}

/*
 * Returns 1 if expression 'expr' contains reference to
 * <s>any variable in the list 'var'.</s>
 * var
 */
bool depends_on(Node *expr, Variable *var) {
	auto vexpr = dynamic_cast<Variable*>(expr);
	if (vexpr) {
		return vexpr->var_name == var->var_name;
	}
	if (is_int(expr) || is_atom(expr))
		return false;
	return depends_on(H(expr), var) || depends_on(T(expr), var);
}

/*
 * Input is a list of pairs [(var_i,expr_i)].
 * Return 1 if any expr_i contains any var_i.
 */
int mutual_recursion(list<pair<Variable*,Node*>> const & l) {
	//list<pair<Variable*,Comb*>> *p,*q = l;

    /*
     * For each expr...
     */
	for (auto e : l) {
		auto n = e.first;
		for (auto e2 : l) {
			auto x = e2.second;
			if (e2.first != n && depends_on(x, n))
				return true;
		}
	}
	return false;
}

Node *abstract(Variable *var,Node *expr) {
	auto vexpr = dynamic_cast<Variable*>(expr);
	if (vexpr && (vexpr->var_name == var->var_name))
		return I;
    if (is_int(expr) || is_var(expr) || is_atom(expr)) {
	return apply(K,expr);
    }
    return apply(apply(S,abstract(var,H(expr))),abstract(var,T(expr)));
}

/*
 * RECURSIVEABSTRACTION := { NAME } '=' EXPR
 *
 * Note: Not currently used.
 */
Node *parse_recursive_abstraction(Variable *name) {
    list<Variable*> names;
    Node *expr;
    /*
     * Parse arguments.
     */
    while (current_token->type!=TOKEN_EQUAL) {
		auto name = parse_name();
		names.push_back(name);//names = apply(name,names);
    }
    /* 
     * Skip '='
     */
    current_token = lex();
    expr = parse_expr();
    while (names.size()) {
		expr = abstract(names.front(),expr);
		names.pop_front();
    }

    if (depends_on(expr,name)) {
    	expr = abstract(name,expr);
    	return apply(Y,expr);
    }
    return expr;
}

/*
 * ABSTRACTION := { NAME } '=' EXPR
 */
Node *parse_abstraction() {
    list<Variable*> names;
    Node *expr;
    /*
     * Parse arguments.
     */
    while (current_token->type!=TOKEN_EQUAL) {
		auto name = parse_name();
		names.push_back(name);//names = apply(name,names);
    }
    /* 
     * Skip '='
     */
    current_token = lex();
    expr = parse_expr();
    display(expr);
    while (names.size()) {
		expr = abstract(names.front(),expr);
		names.pop_front();
    }
    return expr;
}

dictionary defs;

/*
 * DEF := NAME ABSTRACTION '.'
 */
void parse_def() {
    Node **location;
    Variable *name = parse_name();
    Node *expr;
    string var_name = get_var(name);
    location = lookup(defs,var_name);

    expr = parse_abstraction();

    *location = expr;
    expect(TOKEN_PERIOD,"Expected '.' after 'def'");
}

Node *stack_eval(Node *);

struct DisplayVisitor : public CombVisitor {
	void visitComb(Comb* e) { visitDefault(e); }
	void visitSComb(SComb* e) { visitDefault(e); }
	void visitKComb(KComb* e) { visitDefault(e); }
	void visitIComb(IComb* e) { visitDefault(e); }
	void visitUComb(UComb* e) { visitDefault(e); }
	void visitYComb(YComb* e) { visitDefault(e); }
	void visitInt(Int* e) { cout << e->v; }
	void visitPair(Pair* p) {
		Node* e = p;
		cout << '[';
		display(stack_eval(e->head));
		e = T(e);
		while (is_pair(e)) {
			cout << ',';
			display(stack_eval(e->head));
			e = T(e);
		}
		cout << ':';
		display(stack_eval(e));
		cout << ']';
	}
	void visitVariable(Variable* v) {
		cout << "Var(" << v->var_name << ")";
	}
	void visitDefault(Node* e) {
		display(e->head);
		cout << ' ';
		if (!is_comb(T(e))) {
			display(T(e));
		} else {
			cout << '(';
			display(T(e));
			cout << ')';
		}
	}
	void visitCond(Cond* e) {
		visitDefault(e);
	}
	void visitPlusComb(PlusComb* e) { visitDefault(e); }
	void visitMinusComb(MinusComb* e) { visitDefault(e); }
	void visitEqualComb(EqualComb* e) { visitDefault(e); }
	void visitPairComb(PairComb* e) { visitDefault(e); }
	void visitLessComb(LessComb* e) { visitDefault(e); }
	void visitTimesComb(TimesComb* e) { visitDefault(e); }
	void visitHeadComb(HeadComb* e) { visitDefault(e); }
	void visitTailComb(TailComb* e) { visitDefault(e); }
	void visitTrueAtom(TrueAtom* e) { visitDefault(e); }
	void visitFalseAtom(FalseAtom* e) { visitDefault(e); }
	void visitNilComb(NilAtom* e) { visitDefault(e); }
	void visitDivideComb(DivideComb* e) { visitDefault(e); }
	void visitLessEqComb(LessEqComb* e) { visitDefault(e); }
	void visitOrComb(OrComb* e) { visitDefault(e); }
	void visitAndComb(AndComb* e) { visitDefault(e); }
	void visitNotComb(NotComb* e) { visitDefault(e); }
	void visitBComb(BComb* e) { visitDefault(e); }
	void visitCComb(CComb* e) { visitDefault(e); }
	void visitSDashComb(SDashComb* e) { visitDefault(e); }
	void visitBStarComb(BStarComb* e) { visitDefault(e); }
	void visitCDashComb(CDashComb* e) { visitDefault(e); }

};
void display(Node *l) {
	//DisplayVisitor v;
	//l->visit(&v);
	cerr << l->to_string();
}

/*
 * Substitute variables stored in dictionary 'defs' in expressions.
 * Note: this is part of the compilation, not something
 * that happens at run time.
 */
Node *substitute(Node *expr) {
    if (is_comb(expr)) {
		expr->head = substitute(expr->head);
		expr->tail = substitute(expr->tail);
		return expr;
    } else if (is_var(expr)) {
    	auto s = get_var(expr);
    	return *lookup(defs,s);
    } else {
    	return expr;
    }
}

int optimise(Node *);

/*
 * PROGRAM := { DEF } EXPR
 */
Node *parse_program() {
	Node *expr;
	while (current_token->type == TOKEN_DEF) {
		current_token = lex();
		parse_def();
	}
	expr = parse_expr();
	optimise(expr);
	for (auto d : defs) {
		optimise(d.second);
	}
	for (auto d : defs) {
		// This seems shady.
		d.second = substitute(d.second);
	}
	expr = substitute(expr);
	return expr;
}

/*
 * Compare two lists for equality.
 */
int list_eq(Node *a,Node *b);
struct EqualityVisitor : public CombVisitor {
	EqualityVisitor(Node* other) : other(other), result(false) {}
	Node* other;
	bool result;
	void visitComb(Comb*) { }
	void visitSComb(SComb*) { }
	void visitKComb(KComb*) { }
	void visitIComb(IComb*) { }
	void visitUComb(UComb*) { }
	void visitYComb(YComb*) { }
	void visitInt(Int* e) {
		auto pi = dynamic_cast<Int*>(other);
		if (pi) {
			result = pi->v == e->v;
		}
	}
	void visitPair(Pair* p) {
		auto po = dynamic_cast<Pair*>(other);
		if (po) {
			EqualityVisitor v2(other->head);
			EqualityVisitor v3(other->tail);
			result = list_eq(p->head, po->head) && list_eq(p->tail, po->tail);
		}
	}
	void visitVariable(Variable* v) {
		cout << "Var(" << v->var_name << ")";
	}
	void visitCond(Cond*) { }
	void visitPlusComb(PlusComb*) { }
	void visitMinusComb(MinusComb*) { }
	void visitEqualComb(EqualComb*) { }
	void visitPairComb(PairComb*) { }
	void visitLessComb(LessComb*) { }
	void visitTimesComb(TimesComb*) { }
	void visitHeadComb(HeadComb*) { }
	void visitTailComb(TailComb*) { }
	void visitTrueAtom(TrueAtom*) { }
	void visitFalseAtom(FalseAtom*) { }
	void visitNilAtom(NilAtom*) { }
	void visitDivideComb(DivideComb*) { }
	void visitLessEqComb(LessEqComb*) { }
	void visitOrComb(OrComb*) {  }
	void visitAndComb(AndComb*) {  }
	void visitNotComb(NotComb*) { }
	void visitBComb(BComb*) { }
	void visitCComb(CComb*) { }
	void visitSDashComb(SDashComb*) { }
	void visitBStarComb(BStarComb*) { }
	void visitCDashComb(CDashComb*) { }
};
int list_eq(Node *a,Node *b) {
	//if (a->type != b->type) {
	//	return 0;
	//}
	//auto aa = dynamic_cast<Atom*>(a);
	//if (aa) {
	//	auto ba = dynamic_cast<Atom*>(b);
	//	return ba && (get_atom(aa) == get_atom(ba));
	//}
	auto ap = dynamic_cast<Pair*>(a);
	if (ap) {
		auto bp = dynamic_cast<Pair*>(b);
		return bp && (list_eq(H(a), H(b)) && list_eq(T(a), T(b)));
	}
	auto ai = dynamic_cast<Int*>(a);
	if (ai) {
		auto bi = dynamic_cast<Int*>(b);
		return bi && (ai->v == bi->v);
	}
	error_msg("Invalid equality comparison"); return 0;
}

int list_lt(Node *a,Node *b) {
	auto ai = dynamic_cast<Int*>(a);
	if (ai) {
		auto bi = dynamic_cast<Int*>(b);
		if (bi) { return (ai->v < bi->v); }
	}
	error_msg("Can't compare non-integers"); return 0;
}

int list_le(Node *a,Node *b) {
	auto ai = dynamic_cast<Int*>(a);
	if (ai) {
		auto bi = dynamic_cast<Int*>(b);
		if (bi) { return (ai->v <= bi->v); }
	}
	error_msg("Can't compare non-integers"); return 0;
}

/*
 * Implements a->Ia
 * The extra I looks superfulous but has certain uses.
 */
void copy(Node *a,Node *b) {
    *a = is_atom(b) ? *apply(I,b) : *b;
}

/*
 * This implements a number of shortcuts that speed up reductions.
 * They look like reduction rules but note that they are executed
 * at compile time, not reduction time.
 */
int optimise(Node *a) {
    int flag;
    do {
	flag = 0;

	/*
	 * S(Kf)(Kg) -> K(fg)
	 */
	if (is_comb(a)
	    && is_comb(H(a))
	    && is_comb(TH(a))
	    && is_comb(T(a))
	    && HH(a)==S
	    && HTH(a)==K
	    && HT(a)==K) {
		*a = *apply(K,apply(TTH(a),TT(a)));
		flag |= 1;
	}

	/*
	 * S(Kf)I -> f
	 */
	if (is_comb(a)
	    && is_comb(H(a))
	    && is_comb(TH(a))
	    && HH(a)==S
	    && HTH(a)==K
	    && T(a)==I) {
		copy(a,TTH(a));
		flag |= 1;
	}

	/*
	 * S(Kf)(Bgh) -> B*fgh
	 */
	if (is_comb(a)
	    && is_comb(H(a))
	    && is_comb(TH(a))
	    && is_comb(T(a))
	    && is_comb(HT(a))
	    && HH(a)==S
	    && HTH(a)==K
	    && HHT(a)==B) {
		*a = *apply3(Bstar,TTH(a),THT(a),TT(a));
		flag |= 1;
	}

	/*
	 * S(Kf)g->Bfg
	 */
	if (is_comb(a)
	    && is_comb(H(a))
	    && is_comb(TH(a))
	    && HH(a)==S
	    && HTH(a)==K) {
		*a = *apply2(B,TTH(a),T(a));
		flag |= 1;
	}

	/*
	 * S(Bfg)(Kh) -> C'fgh
	 */
	if (is_comb(a)
	    && is_comb(H(a))
	    && is_comb(TH(a))
	    && is_comb(HTH(a))
	    && is_comb(T(a))
	    && HH(a)==S
	    && HHTH(a)==B
	    && HT(a)==K) {
		*a = *apply3(Cdash,THTH(a),TTH(a),TT(a));
		flag |= 1;
	}

	/*
	 * Sf(Kg)->Cfg;
	 */
	if (is_comb(a)
	    && is_comb(H(a))
	    && is_comb(T(a))
	    && HH(a)==S
	    && HT(a)==K) {
		*a = *apply2(C,TH(a),TT(a));
		flag |= 1;
	}

	/*
	 * S(Bfg)h -> S'fgh
	 */
	if (is_comb(a)
	    && is_comb(H(a))
	    && is_comb(TH(a))
	    && is_comb(HTH(a))
	    && HH(a)==S
	    && HHTH(a)==B) {
		*a = *apply3(Sdash,THTH(a),TTH(a),T(a));
		flag |= 1;
	}
	if (is_comb(a)) {
	    flag |= optimise(H(a)) | optimise(T(a));
	}
    } while (flag);

    return 0;
}

struct EvalVisitor : public CombVisitor {
	EvalVisitor(vector<Node*>& stack, int& sp)
	: stack(stack), sp(sp), done(false) {}
	vector<Node*>& stack;
	int& sp;
	bool done;

	void visitComb(Comb*) {
		Node *a = stack[sp];
		++sp;
		stack[sp] = H(a);
		stack[sp - 1] = T(a);
	}
	void visitIComb(IComb*) {
		if (sp < 1)
			return;
		sp--;
	}
	void visitYComb(YComb*) {
		if (sp < 1)
			return;
		Node *a = stack[sp - 1];
		--sp;
		stack[sp] = apply(a, apply(Y, a));
	}
	void visitHeadComb(HeadComb*) {
		if (sp < 1)
			return;
		Node *a = stack[sp - 1];
		--sp;
		a = stack_eval(a);
		if (!is_pair(a)) {
			error_msg("head needs a list");
		}
		stack[sp] = H(a);
	}
	void visitTailComb(TailComb*) {
		if (sp < 1)
			return;
		Node *a = stack[sp - 1];
		--sp;
		a = stack_eval(a);
		if (!is_pair(a)) {
			error_msg("tail needs a list");
		}
		stack[sp] = T(a);
	}
	void visitNotComb(NotComb*) {
		if (sp < 1)
			return;
		Node *a = stack[sp - 1];
		--sp;
		stack[sp] = a == truec ? (Node*)falsec : (Node*)truec;
	}
	void visitKComb(KComb*) {
		if (sp < 2)
			return;
		Node *a = stack[sp - 1];
		sp -= 2;
		stack[sp] = a;
	}
	void visitUComb(UComb*) {
		if (sp < 2)
			return;
		Node *a = stack[sp - 1];
		Node *b = stack[sp - 2];
		stack[sp] = a;
		stack[sp - 1] = apply(head, b);
		stack[sp - 2] = apply(tail, b);
	}
	void visitPlusComb(PlusComb*) {
		if (sp < 2)
			return;
		Node *a = stack[sp - 1];
		Node *b = stack[sp - 2];
		cout << a->to_string() << "+" << b->to_string() << endl;
		a = stack_eval(a);
		b = stack_eval(b);
		cout << a->to_string() << "+" << b->to_string() << endl;
		sp -= 2;
		stack[sp] = make_int(get_int(a) + get_int(b));
		cout << stack[sp]->to_string() << endl;
	}
	void visitMinusComb(MinusComb*) {
		if (sp < 2)
			return;
		Node *a = stack[sp - 1];
		Node *b = stack[sp - 2];
		a = stack_eval(a);
		b = stack_eval(b);
		sp -= 2;
		stack[sp] = make_int(get_int(a) - get_int(b));
	}
	void visitEqualComb(EqualComb*) {
		if (sp < 2)
			return;
		Node *a = stack[sp - 1];
		Node *b = stack[sp - 2];
		a = stack_eval(a);
		b = stack_eval(b);
		sp -= 2;
		stack[sp] = list_eq(a, b) ? (Node*)truec : (Node*)falsec;
	}
	void visitPairComb(PairComb*) {
		if (sp < 2)
			return;
		Node *a = stack[sp - 1];
		Node *b = stack[sp - 2];
		sp -= 2;
		stack[sp] = sapl::make_pair(a, b);
	}
	void visitLessComb(LessComb*) {
		if (sp < 2)
			return;
		Node *a = stack[sp - 1];
		Node *b = stack[sp - 2];
		a = stack_eval(a);
		b = stack_eval(b);
		sp -= 2;
		stack[sp] = list_lt(a, b) ? (Node*)truec : (Node*)falsec;
	}
	void visitTimesComb(TimesComb*) {
		if (sp < 2)
			return;
		Node *a = stack[sp - 1];
		Node *b = stack[sp - 2];
		a = stack_eval(a);
		b = stack_eval(b);
		sp -= 2;
		stack[sp] = make_int(get_int(a) * get_int(b));
	}
	void visitDivideComb(DivideComb*) {
		if (sp < 2)
			return;
		Node *a = stack[sp - 1];
		Node *b = stack[sp - 2];
		cout << a->to_string() << "/" << b->to_string() << endl;
		a = stack_eval(a);
		b = stack_eval(b);
		cout << a->to_string() << "/" << b->to_string() << endl;
		sp -= 2;
		stack[sp] = make_int(get_int(a) / get_int(b));
		cout << stack[sp]->to_string() << endl;
	}
	void visitLessEqComb(LessEqComb*) {
		if (sp < 2)
			return;
		Node *a = stack[sp - 1];
		Node *b = stack[sp - 2];
		a = stack_eval(a);
		b = stack_eval(b);
		sp -= 2;
		stack[sp] = list_le(a, b) ? (Node*)truec : (Node*)falsec;
	}
	void visitOrComb(OrComb*) {
		if (sp < 2)
			return;
		Node *a = stack[sp - 1];
		Node *b = stack[sp - 2];
		a = stack_eval(a);
		sp -= 2;
		stack[sp] =
				(a == truec || stack_eval(b) == truec) ?
						(Node*)truec : (Node*)falsec;
	}
	void visitAndComb(AndComb*) {
		if (sp < 2)
			return;
		Node *a = stack[sp - 1];
		Node *b = stack[sp - 2];
		a = stack_eval(a);
		sp -= 2;
		stack[sp] =
				(a == falsec || stack_eval(b) == falsec) ?
						(Node*)falsec : (Node*)truec;
	}
	void visitSComb(SComb*) {
		if (sp < 3)
			return;
		Node *a = stack[sp - 1];
		Node *b = stack[sp - 2];
		Node *c = stack[sp - 3];
		--sp;
		stack[sp] = a;
		stack[sp - 1] = c;
		stack[sp - 2] = apply(b, c);
	}
	void visitBComb(BComb*) {
		if (sp < 3)
			return;
		Node *a = stack[sp - 1];
		Node *b = stack[sp - 2];
		Node *c = stack[sp - 3];
		sp -= 2;
		stack[sp] = a;
		stack[sp - 1] = apply(b, c);
	}
	void visitCComb(CComb*) {
		if (sp < 3)
			return;
		Node *a = stack[sp - 1];
		Node *b = stack[sp - 2];
		Node *c = stack[sp - 3];
		--sp;
		stack[sp] = a;
		stack[sp - 1] = c;
		stack[sp - 2] = b;
	}
	void visitCond(Cond*) {
		if (sp < 3)
			return;
		Node *a = stack[sp - 1];
		Node *b = stack[sp - 2];
		Node *c = stack[sp - 3];
		a = stack_eval(a);
		sp -= 3;
		if (a == falsec) {
			stack[sp] = c;
		} else if (a == truec) {
			stack[sp] = b;
		} else {
			error_msg(
					"'cond' expects 'true' or 'false'");
		}
	}
	void visitSDashComb(SDashComb*) {
		if (sp < 4)
			return;
		Node *a = stack[sp - 1];
		Node *b = stack[sp - 2];
		Node *c = stack[sp - 3];
		Node *d = stack[sp - 4];
		sp -= 2;
		stack[sp] = a;
		stack[sp - 1] = apply(b, d);
		stack[sp - 2] = apply(c, d);
	}
	void visitBStarComb(BStarComb*) {
		if (sp < 4)
			return;
		Node *a = stack[sp - 1];
		Node *b = stack[sp - 2];
		Node *c = stack[sp - 3];
		Node *d = stack[sp - 4];
		sp -= 3;
		stack[sp] = a;
		stack[sp - 1] = apply(b, apply(c, d));
	}
	void visitCDashComb(CDashComb*) {
		if (sp < 4)
			return;
		Node *a = stack[sp - 1];
		Node *b = stack[sp - 2];
		Node *c = stack[sp - 3];
		Node *d = stack[sp - 4];
		sp -= 2;
		stack[sp] = a;
		stack[sp - 1] = apply(b, d);
		stack[sp - 2] = c;
	}
	void visitTrueAtom(TrueAtom*) { done=true; }
	void visitFalseAtom(FalseAtom*) { done=true; }
	void visitNilAtom(NilAtom*) { done=true; }
	void visitInt(Int*) { done=true; }
	void visitPair(Pair*) { done=true; }
	void visitVariable(Variable*) { done=true; }

};
/*
 * Core combinatorial reduction engine.
 * This is where program execution takes place.
 */
Node *stack_eval(Node *a) {
    vector<Node*> stack;
    stack.resize(1000);
    Node *result;
    int sp = 0;
    int i;

    stack[sp] = a;

    /*
     * Reduction phase
     */
    EvalVisitor eval(stack,sp);
    while (!eval.done) {
    	stack[sp]->visit(&eval);
    }

	/*
	 * Reassemble result
	 */
	result = stack[sp--];
	for (i = sp; i >= 0; --i) {
		result = apply(result, stack[i]);
	}

	/*
	 * If we've evaluated to a combinator expression we should overwrite it.
	 * But we musn't copy atoms as they are unique.
	 */
	if (is_comb(a) && !is_atom(result)) {
		// bad*a = *result;
		//return a;
		return result; // FIXME
	} else {
		return result;
	}
}

/*
 * All of the combinators
 */
void constants() {
    S	    = new SComb();//make_atom('S');
    K	    = new KComb();//amake_atom('K');
    I	    = new IComb();//make_atom('I');
    sapl::plus    = new PlusComb();//make_atom('+');
    sapl::times   = new TimesComb();//make_atom('*');
    sapl::minus   = new MinusComb();//make_atom('-');
    divide  = new DivideComb();//make_atom('/');
    sapl::pair    = new PairComb();//make_atom(':');
    head    = new HeadComb();//make_atom('h');
    tail    = new TailComb();//make_atom('t');
    truec    = new TrueAtom();//make_atom('T');
    falsec   = new FalseAtom();//make_atom('F');
    cond    = new Cond();//make_atom('?');
    U	    = new UComb();//make_atom('U');
    Y	    = new YComb();//make_atom('Y');
    nil	    = new NilAtom();
    sapl::equal   = new EqualComb();
    sapl::less    = new LessComb();
    lesseq  = new LessEqComb();
    cand	    = new AndComb();
    cor	    = new OrComb();
    cnot	    = new NotComb();
    B	    = new BComb();
    C	    = new CComb();
    Sdash   = new SDashComb();
    Bstar   = new BStarComb();
    Cdash   = new CDashComb();
}

void make_keywords() {
    *lookup(keywords,"def")	= (Node *)TOKEN_DEF;
    *lookup(keywords,"if")	= (Node *)TOKEN_IF;
    *lookup(keywords,"then")	= (Node *)TOKEN_THEN;
    *lookup(keywords,"else")	= (Node *)TOKEN_ELSE;
    *lookup(keywords,"where")	= (Node *)TOKEN_WHERE;
    *lookup(keywords,"true")	= (Node *)TOKEN_TRUE;
    *lookup(keywords,"false")	= (Node *)TOKEN_FALSE;
    *lookup(keywords,"nil")	= (Node *)TOKEN_NIL;
    *lookup(keywords,"hd")	= (Node *)TOKEN_HEAD;
    *lookup(keywords,"tl")	= (Node *)TOKEN_TAIL;
    *lookup(keywords,"or")	= (Node *)TOKEN_OR;
    *lookup(keywords,"and")	= (Node *)TOKEN_AND;
    *lookup(keywords,"not")	= (Node *)TOKEN_NOT;
}

/*
 Manage the rerouting of a file's input to the std::cin object
 so that the parser can just use cin.
 */
struct InputFixer
{
	InputFixer() {
		orig_cin = nullptr;
	}
	~InputFixer() {
		if (orig_cin)
			cin.rdbuf(orig_cin);
	}
	int open(const char* name, ios_base::openmode mode) {
		input.open(name,mode);// = fopen(argv[1],"rt");
		if (!input)
			return -1;
		orig_cin = cin.rdbuf(input.rdbuf());
		cin.tie(0);
		return 0;
	}
	ifstream input;
	streambuf* orig_cin;
};
int main(int argc,char **argv) {
	InputFixer inputFixer;
    if (argc>1)
    {
		if (inputFixer.open(argv[1], ios::in))
			return -1;
    }
    Node *t;

    constants();
    make_keywords();
    cerr << "S is_atom is_comb " << is_atom(S) << ' ' << is_comb(S) << endl;
    cerr << "K is_atom is_comb " << is_atom(K) << ' ' << is_comb(K) << endl;
    cerr << "I is_atom is_comb " << is_atom(I) << ' ' << is_comb(I) << endl;
    cerr << "sapl::plus is_atom is_comb " << is_atom(sapl::plus) << ' ' << is_comb(sapl::plus) << endl;
    cerr << "sapl::times is_atom is_comb " << is_atom(sapl::times) << ' ' << is_comb(sapl::times) << endl;
    cerr << "sapl::minus is_atom is_comb " << is_atom(sapl::minus) << ' ' << is_comb(sapl::minus) << endl;
    cerr << "divide is_atom is_comb " << is_atom(divide) << ' ' << is_comb(divide) << endl;
    cerr << "sapl::pair is_atom is_comb " << is_atom(sapl::pair) << ' ' << is_comb(sapl::pair) << endl;
    cerr << "head is_atom is_comb " << is_atom(head) << ' ' << is_comb(head) << endl;
    cerr << "tail is_atom is_comb " << is_atom(tail) << ' ' << is_comb(tail) << endl;
    cerr << "truec is_atom is_comb " << is_atom(truec) << ' ' << is_comb(truec) << endl;
    cerr << "falsec is_atom is_comb " << is_atom(falsec) << ' ' << is_comb(falsec) << endl;
    cerr << "cond is_atom is_comb " << is_atom(cond) << ' ' << is_comb(cond) << endl;
    cerr << "U is_atom is_comb " << is_atom(U) << ' ' << is_comb(U) << endl;
    cerr << "Y is_atom is_comb " << is_atom(Y) << ' ' << is_comb(Y) << endl;
    cerr << "nil is_atom is_comb " << is_atom(nil) << ' ' << is_comb(nil) << endl;
    cerr << "sapl::equal is_atom is_comb " << is_atom(sapl::equal) << ' ' << is_comb(sapl::equal) << endl;
    cerr << "sapl::less is_atom is_comb " << is_atom(sapl::less) << ' ' << is_comb(sapl::less) << endl;
    cerr << "lesseq is_atom is_comb " << is_atom(lesseq) << ' ' << is_comb(lesseq) << endl;
    cerr << "cand is_atom is_comb " << is_atom(cand) << ' ' << is_comb(cand) << endl;
    cerr << "cor is_atom is_comb " << is_atom(cor) << ' ' << is_comb(cor) << endl;
    cerr << "cnot is_atom is_comb " << is_atom(cnot) << ' ' << is_comb(cnot) << endl;
    cerr << "B is_atom is_comb " << is_atom(B) << ' ' << is_comb(B) << endl;
    cerr << "C is_atom is_comb " << is_atom(C) << ' ' << is_comb(C) << endl;
    cerr << "Sdash is_atom is_comb " << is_atom(Sdash) << ' ' << is_comb(Sdash) << endl;
    cerr << "Bstar is_atom is_comb " << is_atom(Bstar) << ' ' << is_comb(Bstar) << endl;
    cerr << "CDash is_atom is_comb " << is_atom(Cdash) << ' ' << is_comb(Cdash) << endl;
    cerr << "Int is_atom is_comb " << is_atom(new Int(1)) << ' ' << is_comb(new Int(1)) << endl;
    cerr << "Pair is_atom is_comb " << is_atom(new Pair(S,K)) << ' ' << is_comb(new Pair(S,K)) << endl;
    cerr << "Var is_atom is_comb " << is_atom(new Variable("N")) << ' ' << is_comb(new Variable("N")) << endl;

    next_char();
    current_token = lex();
    t = parse_program();
    t = stack_eval(t);
    display(t);
    printf("\n");

    return 0;
}
