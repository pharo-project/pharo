RBProgramNode is an abstract class that represents an abstract syntax tree node in a Smalltalk program.

Subclasses must implement the following messages:
	accessing
		start
		stop
	visitor
		acceptVisitor:

The #start and #stop methods are used to find the source that corresponds to this node. "source copyFrom: self start to: self stop" should return the source for this node.

The #acceptVisitor: method is used by RBProgramNodeVisitors (the visitor pattern). This will also require updating all the RBProgramNodeVisitors so that they know of the new node.

Subclasses might also want to redefine match:inContext: and copyInContext: to do parse tree searching and replacing.

Subclasses that contain other nodes should override equalTo:withMapping: to compare nodes while ignoring renaming temporary variables, and children that returns a collection of our children nodes.

Instance Variables:
	comments	<Collection of: Interval>	the intervals in the source that have comments for this node
	parent	<RBProgramNode>	the node we're contained in

Shared Variables:
	FormatterClass	<Behavior>	the formatter class that is used when we are formatted