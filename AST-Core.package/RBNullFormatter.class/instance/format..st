public interface
format: aParseTree 
	^ aParseTree source
		ifNil: [	self visitNode: aParseTree.
					codeStream contents ]