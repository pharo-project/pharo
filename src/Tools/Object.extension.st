Extension { #name : #Object }

{ #category : #'*Tools' }
Object >> inspector [

	"Create and schedule an Inspector in which the user can examine the receiver's variables.
	Return the inspector. For getting the object itself, use >>inspect"

	"https://github.com/pharo-project/pharo/issues/11803"

	^Smalltalk tools inspector inspect: self
]

{ #category : #'*Tools-FileList' }
Object class >> services [
	"Backstop"
	^#()
]
