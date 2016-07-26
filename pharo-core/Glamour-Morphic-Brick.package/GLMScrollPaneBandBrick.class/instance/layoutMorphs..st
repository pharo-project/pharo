accessing
layoutMorphs: aBoolean
	"if you know, that morphs are used in scrollpane set me to true.
	each time layout changes I relayout all morphs after some delay
	implemented using async task, such that it can be canceled"
	shouldLayoutMorphs := aBoolean 