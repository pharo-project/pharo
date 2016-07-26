private
selectedWithoutNotifyingComplexContents: aBoolean
	"Only called at creation"
	
	aBoolean ifFalse: [ ^ self ].
	
	container listManager silentlySetSelectedMorph: self.
	selected := aBoolean.