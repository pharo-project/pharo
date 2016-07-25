private
apply: classModification

	classModification modifiesSlots
		ifTrue: [self applySlotChange: classModification]
		ifFalse: [ 
			classModification modifiesFormat
				ifTrue: [ self applyFormatChange: classModification]
				ifFalse: [
					classModification modifiesSuperclass
						ifTrue: [ self applySuperclassChange: classModification ]
						ifFalse: [ 
							(classModification modifiesSharedPools or: [ classModification modifiesClassVariables ])
								ifTrue: [ self applySharedVariableOrPoolChange: classModification ]
								ifFalse: [ 
									"Note: In the other cases of this evil if nesting, the trait composition is set
									by the installer."
									^ self installTraitComposition: classModification ] ]]].
	
	self 
		migrateClasses: classModification;
		cleanUp