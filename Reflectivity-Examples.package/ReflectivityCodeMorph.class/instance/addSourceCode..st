accessing
addSourceCode: aCompiledMethod
	|editor|
	
	self compiledMethod: aCompiledMethod.
	
	editor := self newScrolledText.
	editor hResizing: #spaceFill; vResizing: #spaceFill.
	
	self textModel setText: aCompiledMethod sourceCode.
	self addMorph: editor.