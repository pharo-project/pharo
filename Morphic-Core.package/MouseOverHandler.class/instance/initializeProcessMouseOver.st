event handling - private
initializeProcessMouseOver

	leftMorphs := mouseOverMorphs asIdentitySet.
	overMorphs := WriteStream on: (Array new: leftMorphs size).
	enteredMorphs := WriteStream on: #()