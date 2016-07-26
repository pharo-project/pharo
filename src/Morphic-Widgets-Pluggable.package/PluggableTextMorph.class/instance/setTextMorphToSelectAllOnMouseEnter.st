editor access
setTextMorphToSelectAllOnMouseEnter
	"Tell my textMorph's editor to select all when the mouse enters"

	textMorph on: #mouseEnter send: #selectAll to: textMorph