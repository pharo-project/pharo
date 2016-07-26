accessing-classes
createNewClassFor: aBehavior 
	| nonMeta meta className |
	className := aBehavior theNonMetaClass name.
	nonMeta := (RBClass existingNamed: className)
				model: self;
				yourself.
	meta := (RBMetaclass existingNamed: className)
				model: self;
				yourself.
	^changedClasses at: className put: (Array with: nonMeta with: meta)