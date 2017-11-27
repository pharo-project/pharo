Extension { #name : #Behavior }

{ #category : #'*Slot' }
Behavior >> classLayout [
	^ layout
		ifNil: [ 
			| superLayout scope |
			superLayout := superclass classLayout.
			scope := superLayout slotScope extend.
			layout := superLayout class extending: superLayout scope: scope host: self ]
]

{ #category : #'*Slot' }
Behavior >> classLayout: aClassLayout [
	layout := aClassLayout
]

{ #category : #'*Slot' }
Behavior >> definesSlot: aSlot [
	"Return true whether the receiver defines an instance variable named aString"
	
	^ self slots identityIncludes: aSlot
]

{ #category : #'*Slot' }
Behavior >> definesSlotNamed: aString [
	"Return true whether the receiver defines an instance variable named aString."
	
	^ self slotNames includes: aString
]
