subclass creation
subclass: t uses: aTraitComposition 
	| cls |
	cls := self
				subclass: t
				instanceVariableNames: ''
				classVariableNames: ''
				poolDictionaries: ''
				category: 'Unclassified'.
	cls setTraitComposition: aTraitComposition asTraitComposition.
	^ cls