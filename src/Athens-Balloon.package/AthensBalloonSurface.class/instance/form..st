accessing
form: aForm

	form := aForm.
	mask := Form extent: form extent depth: 1. 

	engine := AthensBalloonEngine new.
	engine bitBlt: (GrafPort toForm: form) .
	engine clipRect: (0@0 extent: form extent).
	engine edgeTransform: pathTransform.
	engine destOffset: 0@0.
	engine reset.