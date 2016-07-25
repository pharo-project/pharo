private
setDestForm: df 
	| bb |
	bb := df boundingBox.
	destForm := df.
	clipX := bb left.
	clipY := bb top.
	clipWidth := bb width.
	clipHeight := bb height