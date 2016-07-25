adding/removing
addClass: aRGBehaviorDefinition
	"aRGBehaviorDefinition has to be a class, trait or metaclass"
	self flag: 'when i am adding a metaclass? check this?'.

	(aRGBehaviorDefinition isClass
		or:[ aRGBehaviorDefinition isTrait ]) ifFalse:[ ^self ].
	self addElement: aRGBehaviorDefinition in: self definedClasses