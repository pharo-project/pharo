accessing
superclass: aRGBehaviorDefinition
	"The superclass is assigned.
	If aRGBehaviorDefinition is not nil the receiver is added as a subclass and the superclass assignment also happens for theMetaClass"

	superclass := aRGBehaviorDefinition.
	superclass notNil
		ifFalse: [ ^ self ].
	self superclassName: aRGBehaviorDefinition name.
	aRGBehaviorDefinition addSubclass: self.
	self hasMetaclass
		ifTrue: [ self theMetaClass superclass: aRGBehaviorDefinition theMetaClass ]