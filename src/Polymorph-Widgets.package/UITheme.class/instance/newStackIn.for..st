morph creation
newStackIn: aThemedMorph for: controls
	"Answer a morph laid out with a stack of controls."

	|answer|
	answer := PanelMorph new
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		fillStyle: Color transparent; "non pane color tracking"
		layoutPolicy: StackLayout new.
	controls do: [:m | answer addMorphBack: m].
	^answer
			