opening
open: aPopupBrick with: aRectangle
	"In case of downward direction we want popup's content to be below trigger brick"

	^ aPopupBrick openBelow: aRectangle