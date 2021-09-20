Extension { #name : #Object }

{ #category : #'*Tools' }
Object >> inspector [
	^ Smalltalk tools inspector inspector: self
]

{ #category : #'*Tools-FileList' }
Object class >> services [
	"Backstop"
	^#()
]
