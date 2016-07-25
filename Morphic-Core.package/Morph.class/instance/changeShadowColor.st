drop shadows
changeShadowColor
	"Change the shadow color of the receiver -- triggered, e.g. from a menu"

	(UIManager default chooseColor: self shadowColor)
		ifNotNil: [:nc | self shadowColor: nc].
