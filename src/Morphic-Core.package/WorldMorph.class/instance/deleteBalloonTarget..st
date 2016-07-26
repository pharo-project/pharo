menu & halo
deleteBalloonTarget: aMorph
	"Delete the balloon help targeting the given morph"

	self handsDo: [ :h | h deleteBalloonTarget: aMorph ]