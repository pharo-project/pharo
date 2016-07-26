accessing
methodsForSelectedClassCategory
	| visibleClasses |
	visibleClasses := self visibleClasses.
	^ items select: [:ea | (visibleClasses includes: ea className) 
								and: [ea isMethodDefinition
									and: [ea classIsMeta = self switchIsClass]]].