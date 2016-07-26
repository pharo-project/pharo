public interface
format: aParseTree
	originalSource := aParseTree source.
	self visitNode: aParseTree.
	^ codeStream contents