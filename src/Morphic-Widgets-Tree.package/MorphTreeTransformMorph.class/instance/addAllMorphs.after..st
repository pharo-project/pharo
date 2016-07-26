submorphs-add/remove
addAllMorphs: aCollection after: anotherMorph
	^self privateAddAllMorphs: aCollection 
			atIndex: (anotherMorph index ifNil: [submorphs size])