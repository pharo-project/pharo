event handling
selectIndexAtPoint: aPoint
	| yPos  |
	yPos := aPoint y - self bounds top.
	selected := firstVisible + (yPos / self class itemHeight) floor.
	selected := selected min: context model entries size max: 1.