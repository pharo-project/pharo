Extension { #name : 'SmallFloat64' }

{ #category : '*DeepCopier' }
SmallFloat64 >> veryDeepCopyWith: deepCopier [
	"Answer the receiver, because SmallFloat64s are unique."
	^self
]
