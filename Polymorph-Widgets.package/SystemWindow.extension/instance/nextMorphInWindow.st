*Polymorph-Widgets
nextMorphInWindow
	"Answer the next morph in the window. Traverse
	from the receiver to its first pane."

	^self hasSubmorphs
		ifTrue: [self submorphs first]