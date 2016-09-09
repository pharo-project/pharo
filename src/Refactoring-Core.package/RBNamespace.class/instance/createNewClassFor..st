accessing-classes
createNewClassFor: aBehavior 
	| nonMeta meta className |
	className := aBehavior theNonMetaClass name.
	nonMeta := (RBClassModelFactory rbClass existingNamed: className)
				model: self;
				yourself.
	meta := (RBClassModelFactory rbMetaclass existingNamed: className)
				model: self;
				yourself.
	^changedClasses at: className put: (Array with: nonMeta with: meta)