module vayne.ast.node;


import std.traits;

import vayne.ast.visitor;
import vayne.source.token;


mixin template VisitorInterface() {
	override void accept(IVisitor visitor) {
		visitor.visit(this);
	}
}


class Node : IVisitorNode {
	this(Args...)(Token tok, Args args) {
		this.tok = tok;

		static if (args.length) {
			static if ((args.length == 1) && isArray!(Args[0])) {
				children = args[0];
			} else {
				children.reserve(args.length);
				foreach (arg; args)
					children ~= arg;
			}
		}
	}

	override void accept(IVisitor visitor) {
		assert(false, "unsupported");
	}

	Token tok;
	Node[] children;
}


class Expression : Node {
	this(Token tok, Node expr, bool parens = false) {
		super(tok, expr);

		parenthesis = parens;
	}

	mixin VisitorInterface;

	bool parenthesis;
}


class Constant : Node {
	this(Token tok) {
		super(tok);
	}

	mixin VisitorInterface;
}


class Identifier : Node {
	this(Token tok) {
		super(tok);
	}

	mixin VisitorInterface;
}


class UnaryOp : Node {
	this(Token tok, Node expr) {
		super(tok, expr);
	}

	mixin VisitorInterface;
}


class BinaryOp : Node {
	this(Token tok, Node lexpr, Node rexpr) {
		super(tok, lexpr, rexpr);
	}

	mixin VisitorInterface;
}


class ConditionalExpression : Node {
	this(Token tok, Node condition, Node trueCase, Node falseCase) {
		super(tok, condition, trueCase, falseCase);
	}

	mixin VisitorInterface;
}


class IndexOp : Node {
	this(Token tok, Node expr, Node index) {
		super(tok, expr, index);
	}

	mixin VisitorInterface;
}


class SliceOp : Node {
	this(Token tok, Node expr, Node start, Node end) {
		super(tok, expr, start, end);
	}

	mixin VisitorInterface;
}


class DispatchOp : Node {
	this(Token tok, Node expr, Token target) {
		super(tok, expr);

		this.target = target;
	}

	mixin VisitorInterface;

	Token target;
}


class FunctionCall : Node {
	this(Token tok, Node call, Node[] args) {
		super(tok, call ~ args);
	}

	mixin VisitorInterface;
}


class Output : Node {
	this(Token tok, Node expr) {
		super(tok, expr);
	}

	mixin VisitorInterface;
}


class StatementBlock : Node {
	this(Token tok, Node[] children = null) {
		super(tok, children);
	}

	mixin VisitorInterface;
}


class WithStatement : Node {
	this(Token tok, Node[] exprs, Node body_) {
		super(tok, exprs ~ body_);
	}

	mixin VisitorInterface;
}


class WithExpression : Node {
	this(Token tok, Node expr, Token name) {
		super(tok, expr);

		this.name = name;
	}

	mixin VisitorInterface;

	Token name;
}


class IfStatement : Node {
	this(Token tok, Node expr, Node trueCase, Node elseCase) {
		super(tok, expr, trueCase, elseCase);
	}

	mixin VisitorInterface;
}


class LoopStatement : Node {
	this(Token tok, Token key, Token value, Node obj, Node end, Node body_) {
		super(tok, obj, end, body_);

		this.key = key;
		this.value = value;
	}

	mixin VisitorInterface;

	Token key;
	Token value;
}


class Module : Node {
	this(Token tok, Node[] children) {
		super(tok, children);
	}

	mixin VisitorInterface;
}


auto create(T, Args...)(Args args) {
	return new T(args);
}
