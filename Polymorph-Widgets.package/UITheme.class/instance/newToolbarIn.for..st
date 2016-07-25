morph creation
newToolbarIn: aThemedMorph for: controls
	"Answer a new toolbar with the given controls."

	|answer|
	answer := self newToolbarIn: aThemedMorph.
	controls do: [:m | answer addMorphBack: m].
	^answer