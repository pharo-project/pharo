initialize-release
uses: aTraitCompositionOrArray instanceVariableNames: instVarString 
	| newComposition newMetaClass copyOfOldMetaClass |
	
	copyOfOldMetaClass := self copy.
	newMetaClass := self instanceVariableNames: instVarString.
	
	newComposition := aTraitCompositionOrArray asTraitComposition.
	newMetaClass assertConsistantCompositionsForNew: newComposition.
	newMetaClass setTraitComposition: newComposition.
	
	SystemAnnouncer uniqueInstance
		classDefinitionChangedFrom: copyOfOldMetaClass to: newMetaClass