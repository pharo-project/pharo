running
basicCheck: anAST

	anAST pragmaNamed: #gtInspectorPresentationOrder: ifPresent: [ 
	^ (anAST sendNodes allSatisfy: [ :message |
		(self presentationScriptingMathods includes: message selector) ==> [
			| argument |
			argument := message arguments first.
			argument isBlock or: [ argument isLiteralNode ] ] ]) ].

	^ false