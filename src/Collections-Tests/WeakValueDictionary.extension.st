Extension { #name : #WeakValueDictionary }

{ #category : #'*Collections-Tests' }
WeakValueDictionary >> privateAssociations [
	"I am a method used for test to return the WeakValueAssociations instead of Associations."

	^ array select: [ :each | each value isNotNil ]
]
