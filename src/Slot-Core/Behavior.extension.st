Extension { #name : #Behavior }

{ #category : #'*Slot-Core' }
Behavior >> classLayout [
	^ layout
		ifNil: [ 
			| superLayout scope |
			superLayout := superclass 
				ifNil: [ FixedLayout new slotScope: LayoutEmptyScope instance] "happend in the bootrap"
				ifNotNil: [:sl | sl classLayout].
			scope := superLayout slotScope extend.
			layout := superLayout class extending: superLayout scope: scope host: self ]
]

{ #category : #'*Slot-Core' }
Behavior >> classLayout: aClassLayout [
	layout := aClassLayout
]

{ #category : #'*Slot-Core' }
Behavior >> definesSlot: aSlot [
	"Return true whether the receiver defines an instance variable named aString"
	
	^ self slots identityIncludes: aSlot
]

{ #category : #'*Slot-Core' }
Behavior >> definesSlotNamed: aString [
	"Return true whether the receiver defines an instance variable named aString."
	
	^ self slotNames includes: aString
]
