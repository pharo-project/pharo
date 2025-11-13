Extension { #name : 'Character' }

{ #category : '*System-Model' }
Character >> veryDeepCopyWith: deepCopier [
	"Answer the receiver, because Characters are unique."
	^self
]
