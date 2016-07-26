RBReplaceRule is the abstract superclass of all of the transforming rules. The rules change the source code by replacing the node that matches the rule. Subclasses implement different strategies for this replacement.

Subclasses must implement the following messages:
	matching
		foundMatchFor:

Instance Variables:
	verificationBlock	<BlockClosure>	Is evaluated with the matching node. This allows for further verification of a match beyond simple tree matching.

