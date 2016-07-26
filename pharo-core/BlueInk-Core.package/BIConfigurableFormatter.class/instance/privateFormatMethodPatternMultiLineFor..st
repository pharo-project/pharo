private
privateFormatMethodPatternMultiLineFor: aMethodNode
	| selectors arguments |
	arguments := aMethodNode arguments.
	selectors := aMethodNode selectorParts.
	codeStream nextPutAll: selectors first.
	self space.
	self visitNode: arguments first.
	codeStream nextPut: Character cr.
	self
		with: selectors allButFirst
		and: arguments allButFirst
		do: [ :key :arg | 
			codeStream nextPutAll: self indentString.
			codeStream nextPutAll: key.
			self space.
			self visitNode: arg ]
		separatedBy: (self characterSeparatorMethodSignatureFor: aMethodNode).
	self newLines: 1