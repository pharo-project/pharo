submorphs-add/remove
addAllMorphs: aCollection after: anotherMorph
	^self privateAddAllMorphs: aCollection 
			atIndex: (submorphs indexOf: anotherMorph ifAbsent: [submorphs size])