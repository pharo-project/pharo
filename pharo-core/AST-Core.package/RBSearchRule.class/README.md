RBSearchRule is a parse tree rule that simply searches for matches to the rule. Every time a match is found, answerBlock is evaluated with the node that matches and the cureent answer. This two-argument approach allows a collection to be formed from all of the matches (Think inject:into:).

Instance Variables:
	answerBlock	<BlockClosure>	Block to evaluate with the matching node and the current answer.

