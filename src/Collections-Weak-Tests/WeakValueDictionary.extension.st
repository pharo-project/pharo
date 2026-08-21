Extension { #name : 'WeakValueDictionary' }

{ #category : '*Collections-Weak-Tests' }
WeakValueDictionary >> privateAssociations [
	"I am a method used for test to return the WeakValueAssociations instead of Associations."

	^ self array select: [ :each | each value isNotNil ]
]
