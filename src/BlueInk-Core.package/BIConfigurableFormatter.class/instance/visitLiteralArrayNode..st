visiting
visitLiteralArrayNode: aRBArrayLiteralNode 
	| brackets |
	codeStream nextPut: $#.
	brackets := aRBArrayLiteralNode isForByteArray 
				ifTrue: ['[]']
				ifFalse: ['()'].
	self bracketWith: brackets
		around: 
			[aRBArrayLiteralNode contents do: [:each | self visitNode: each]
				separatedBy: [self space]]