A selectionMorph supports the selection of multiple objects in a morphic world or pasteUp. Using command+shift you get a squared lazzo and you can grab morphs. After you can grab the selection as a morph.

Structure:
	selectedItems	an OrderedCollection of Morphs
					These are the morphs that have been selected
	slippage		a Point
					Keeps track of actual movement between the 
					steps of gridded movement
	dupLoc		a Point
					Notes the position when first duplicate request occurs from halo
	dupDelta	a Point
					Holds the final delta of the first duplicate plus subsequent moves.
