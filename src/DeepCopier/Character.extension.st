Extension { #name : 'Character' }

{ #category : '*DeepCopier' }
Character >> veryDeepCopyWith: deepCopier [
	"Answer the receiver, because Characters are unique."
	^self
]
