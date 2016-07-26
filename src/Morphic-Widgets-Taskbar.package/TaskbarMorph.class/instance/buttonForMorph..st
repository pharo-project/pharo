taskbar
buttonForMorph: aMorph
	"Answer the button corresonding to the given
	morph or nil if none."
	
	|index|
	index := (self orderedTasks collect: [:t | t morph]) indexOf: aMorph.
	^index = 0 ifTrue: [nil] ifFalse: [self submorphs at: index ifAbsent: []]