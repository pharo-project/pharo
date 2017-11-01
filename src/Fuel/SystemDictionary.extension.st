Extension { #name : #SystemDictionary }

{ #category : #'*Fuel' }
SystemDictionary >> fuelAccept: aGeneralMapper [
	"serialize all instances except for <Smalltalk globals>"
	^ self == Smalltalk globals 
		ifFalse: [ super fuelAccept: aGeneralMapper ]
		ifTrue: [ aGeneralMapper visitHookPrimitive: self ]
]

{ #category : #'*Fuel' }
SystemDictionary class >> materializeFrom: aDecoder [
	"Answer my well-known instance"

	^ Smalltalk globals
]

{ #category : #'*Fuel' }
SystemDictionary >> serializeOn: anEncoder [
	"Do nothing"
]
