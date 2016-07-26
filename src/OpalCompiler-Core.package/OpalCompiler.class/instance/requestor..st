accessing
requestor: aRequestor
	self compilationContext requestor: aRequestor.
	self compilationContext interactive: (UIManager default interactiveParserFor: aRequestor).