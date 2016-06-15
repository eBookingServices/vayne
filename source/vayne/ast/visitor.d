module vayne.ast.visitor;


import vayne.ast.node;


interface IVisitorNode {
	void accept(IVisitor);
}


mixin template NodeInterface(T) {
	void visit(T);
}


interface IVisitor {
	mixin NodeInterface!Expression;
	mixin NodeInterface!Constant;
	mixin NodeInterface!Identifier;
	mixin NodeInterface!UnaryOp;
	mixin NodeInterface!BinaryOp;
	mixin NodeInterface!ConditionalExpression;
	mixin NodeInterface!IndexOp;
	mixin NodeInterface!DispatchOp;
	mixin NodeInterface!FunctionCall;
	mixin NodeInterface!WithStatement;
	mixin NodeInterface!IfStatement;
	mixin NodeInterface!LoopStatement;
	mixin NodeInterface!Output;
	mixin NodeInterface!StatementBlock;
	mixin NodeInterface!Module;
}


mixin template VisitNode(T) {
	override void visit(T node) {
		foreach (child; node.children) {
			if (child)
				child.accept(this);
		}
	}
}


class Visitor : IVisitor {
	mixin VisitNode!Expression;
	mixin VisitNode!Constant;
	mixin VisitNode!Identifier;
	mixin VisitNode!UnaryOp;
	mixin VisitNode!BinaryOp;
	mixin VisitNode!ConditionalExpression;
	mixin VisitNode!IndexOp;
	mixin VisitNode!DispatchOp;
	mixin VisitNode!FunctionCall;
	mixin VisitNode!WithStatement;
	mixin VisitNode!IfStatement;
	mixin VisitNode!LoopStatement;
	mixin VisitNode!Output;
	mixin VisitNode!StatementBlock;
	mixin VisitNode!Module;
}
