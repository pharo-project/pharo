*Polymorph-Widgets
optimalExtent
	| optimalWidth optimalHeight |
	optimalWidth := self hUnadjustedScrollRange + 20.
	optimalHeight := (self getListSize max: 1) * font height.
	^ optimalWidth @ optimalHeight
