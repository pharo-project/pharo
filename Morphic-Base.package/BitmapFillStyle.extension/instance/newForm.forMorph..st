*Morphic-Base-Balloon
newForm: aForm forMorph: aMorph
	self form: aForm.
	self direction: (aForm width @ 0).
	self normal: (0 @ aForm height).
	aMorph changed.