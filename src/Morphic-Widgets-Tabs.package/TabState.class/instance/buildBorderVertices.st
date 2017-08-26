protocol
buildBorderVertices
	| borderVertices |
	
	borderVertices := OrderedCollection new.
	borderVertices
		add: tab bottomLeft;
		add: tab topLeft" + (7 @ 0)";
		add: tab topRight" + (-8 @ 0)";
		add: tab bottomRight" + (-1@0)".

	^ borderVertices