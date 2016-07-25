as yet unclassified
newMenuWith: morphs
	"Answer menu with the given morphs."

	^(self newEmbeddedMenu addAllMorphs: morphs)
		borderWidth: 0;
		removeDropShadow;
		color: Color transparent;
		hResizing: #spaceFill;
		cornerStyle: #square;
		stayUp: true;
		beSticky;
		popUpOwner: (MenuItemMorph new privateOwner: self)