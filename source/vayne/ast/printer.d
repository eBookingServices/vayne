module vayne.ast.printer;


import std.array;

import vayne.ast.node;
import vayne.ast.visitor;


class Printer(Appender) : Visitor {
	override void visit(Expression node) {
		if (node.parenthesis)
			app.put("(");

		super.visit(node);

		if (node.parenthesis)
			app.put(")");
	}

	override void visit(Constant node) {
		app.put(node.tok.toString);
	}

	override void visit(Identifier node) {
		app.put(node.tok.toString);
	}

	override void visit(UnaryOp node) {
		app.put(node.tok.toString);

		node.children[0].accept(this);
	}

	override void visit(BinaryOp node) {
		node.children[0].accept(this);
		app.put(" ");
		app.put(node.tok.toString);
		app.put(" ");
		node.children[1].accept(this);
	}

	override void visit(ConditionalExpression node) {
		node.children[0].accept(this);
		app.put("?");
		node.children[1].accept(this);
		app.put(":");
		node.children[2].accept(this);
	}

	override void visit(IndexOp node) {
		node.children[0].accept(this);
		app.put("[");
		node.children[1].accept(this);
		app.put("]");
	}

	override void visit(SliceOp node) {
		node.children[0].accept(this);
		app.put("[");
		node.children[1].accept(this);
		app.put("..");
		node.children[2].accept(this);
		app.put("]");
	}

	override void visit(DispatchOp node) {
		node.children[0].accept(this);
		app.put(".");
		app.put(node.target.value);
	}

	override void visit(FunctionCall node) {
		node.children[0].accept(this);
		app.put("(");
		foreach (i, child; node.children[1..$]) {
			child.accept(this);
			if ((i + 2) != node.children.length)
				app.put(",");
		}
		app.put(")");
	}

	override void visit(WithExpression node) {
		node.children[0].accept(this);
		if (!node.name.empty) {
			app.put(" as ");
			app.put(node.name.value);
		}
	}

	override void visit(WithStatement node) {
		app.put("with(");
		foreach (i, child; node.children[0..$-1]) {
			child.accept(this);
			if ((i + 2) != node.children.length)
				app.put(",");
		}
		app.put(")");
		node.children[$-1].accept(this);
	}

	override void visit(IfStatement node) {
		app.put("if(");
		node.children[0].accept(this);
		app.put(")");
		node.children[1].accept(this);

		if (node.children[2] !is null) {
			app.put("else");
			node.children[2].accept(this);
		}
	}

	override void visit(Output node) {
		app.put("out(");
		node.children[0].accept(this);
		app.put(")");
	}

	override void visit(LoopStatement node) {
		app.put("foreach(");
		if (!node.key.empty) {
			app.put(node.key.value);
			app.put(',');
		}
		app.put(node.value.value);
		app.put(';');

		node.children[0].accept(this);
		if (node.children[1] !is null) {
			app.put("..");
			node.children[1].accept(this);
		}
		app.put(")");

		node.children[2].accept(this);
	}

	override void visit(StatementBlock node) {
		app.put("{");
		foreach (child; node.children) {
			child.accept(this);
			app.put(";");
		}
		app.put("}");
	}

	override void visit(Module node) {
		foreach (child; node.children) {
			child.accept(this);
			app.put(";");
		}
	}

	void print(ref Appender app, Node root) {
		this.app = &app;

		root.accept(this);
	}

	private Appender* app;
}


string print(Node root) {
	auto app = appender!string;
	auto printer = new Printer!(typeof(app));
	printer.print(app, root);
	return app.data;
}
