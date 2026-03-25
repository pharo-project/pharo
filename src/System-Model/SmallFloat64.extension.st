Extension { #name : 'SmallFloat64' }

{ #category : '*System-Model' }
SmallFloat64 >> veryDeepCopyWith: deepCopier [
	"Answer the receiver, because SmallFloat64s are unique."
	^self
]
