Extension { #name : #Object }

{ #category : #'*Tools-Debugger' }
Object class >> canonicalArgumentName [

	| prefix |
	prefix := self name first isVowel
		ifTrue: [ 'an' ]
		ifFalse: [ 'a' ].
	^ prefix, self name.
]

{ #category : #'*Tools-FileList' }
Object class >> services [
	"Backstop"
	^#()
]
