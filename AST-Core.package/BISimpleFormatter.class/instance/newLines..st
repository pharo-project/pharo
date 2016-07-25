private
newLines: anInteger
	anInteger + self indentString size = 0
		ifTrue: 
			[ codeStream space ].
	anInteger
		timesRepeat: 
			[ codeStream cr ].
	lineStart := codeStream position.
	self
		indent
		timesRepeat: 
			[ codeStream nextPutAll: self indentString ]