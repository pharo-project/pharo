Extension { #name : #TextIndent }

{ #category : #'*Text-Scanning' }
TextIndent >> emphasizeScanner: scanner [
	scanner indentationLevel: amount
]
