accessing
calculateNewBounds
	|   position width font |

	"the top point in view is relative to view, 
	since morphic uses absolute coordinates we should add view position here"
	position := layoutPosition textPosition.
	font := (position span style at: TxFontAttribute) value.
	width := font widthOf: position nextPosition getCharacter.
	^ (owner layoutViewMorph topLeft + layoutPosition topPointInView) floor extent: width ceiling @ layoutPosition line height ceiling.