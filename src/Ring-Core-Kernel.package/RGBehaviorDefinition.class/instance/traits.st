accessing
traits
	"Retrieves ring traits based on the names in the traitComposition and from the environment if it is a ring object"
	
	^ self environment isRingObject 	
		ifTrue: [ self traitNames collect:[ :each| self environment traitNamed: each ] ]
		ifFalse:[ #() ]