accessing
codeEditing: aBoolean
	codeEditing = aBoolean ifTrue: [^self].
	codeEditing := aBoolean.
	self changed: #code.
