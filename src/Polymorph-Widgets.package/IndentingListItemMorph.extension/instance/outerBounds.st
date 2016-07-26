*Polymorph-Widgets
outerBounds
	"Return the 'outer' bounds of the receiver, e.g., the bounds that need to be invalidated when the receiver changes."
	
	|box|
	box := super outerBounds.
	container ifNil: [^box].
	^box left: (box left min: self selectionFrame left)