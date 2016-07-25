*Morphic-Base
changeColorIn: aMorph event: evt
	"Note: This is just a workaround to make sure we don't use the old color inst var"
	aMorph changeColorTarget: aMorph selector: #fillStyle: originalColor: self hand: evt hand