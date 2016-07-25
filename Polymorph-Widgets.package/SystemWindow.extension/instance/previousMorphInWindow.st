*Polymorph-Widgets
previousMorphInWindow
	"Answer the previous morph in the window. This will be the
	last submorph recursively of the first pane morph."

	^self hasSubmorphs
		ifTrue: [self lastSubmorphRecursive]