displaying
forceDamageToScreen: allDamage
	"Force all the damage rects to the screen."
	| regions rectList |
	rectList := allDamage.
	"Note: Reset extra regions at the beginning to prevent repeated errors"
	regions := extraRegions.
	extraRegions := nil.
	regions ifNotNil:[
		"exclude extra regions"
		regions do:[:drawerAndRect| | excluded remaining |
			excluded := drawerAndRect at: 2.
			remaining := Array new writeStream.
			rectList do:[:r|
				remaining nextPutAll:(r areasOutside: excluded)].
			rectList := remaining contents].
	].
	rectList do:[:r| self forceToScreen: r].
	regions ifNotNil:[
		"Have the drawers paint what is needed"
		regions do:[:drawerAndRect| (drawerAndRect at: 1) forceToScreen].
	].