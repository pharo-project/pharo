RBValueNode is an abstract class that represents a node that returns some value.

Subclasses must implement the following messages:
	accessing
		startWithoutParentheses
		stopWithoutParentheses
	testing
		needsParenthesis

Instance Variables:
	parentheses	<SequenceableCollection of: Inteval>	the positions of the parethesis around this node. We need a collection of intervals for stupid code such as "((3 + 4))" that has multiple parethesis around the same expression.

