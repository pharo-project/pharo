Extension { #name : 'SystemEnvironment' }

{ #category : '*Fuel-Core' }
SystemEnvironment >> fuelAccept: aGeneralMapper [
	"serialize all instances except for <Smalltalk globals>"
	^ self == Smalltalk globals 
		ifFalse: [ super fuelAccept: aGeneralMapper ]
		ifTrue: [ aGeneralMapper visitHookPrimitive: self ]
]

{ #category : '*Fuel-Core' }
SystemEnvironment class >> materializeFrom: aDecoder [
	"Answer my well-known instance"

	^ Smalltalk globals
]

{ #category : '*Fuel-Core' }
SystemEnvironment >> serializeOn: anEncoder [
	"Do nothing"
]
