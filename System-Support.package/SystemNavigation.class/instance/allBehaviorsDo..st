query
allBehaviorsDo: aBlock
	"Execute a block on each class, metaclass, trait and trait class"

	self environment allClassesDo: [ :aClass | 
		aBlock
			value: aClass;
			value: aClass theMetaClass ].

	self environment allTraitsDo: [ :aTrait | 
		aBlock
			value: aTrait;
			value: aTrait theMetaClass ]