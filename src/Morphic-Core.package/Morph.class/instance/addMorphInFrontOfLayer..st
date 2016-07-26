wiw support
addMorphInFrontOfLayer: aMorph

	| targetLayer |

	targetLayer := aMorph morphicLayerNumber.
	submorphs do: [ :each | | layerHere |
		each == aMorph ifTrue: [^self].
		layerHere := each morphicLayerNumber.
		"the <= is the difference - it insures we go to the front of our layer"
		targetLayer <= layerHere ifTrue: [
			^self addMorph: aMorph inFrontOf: each
		].
	].
	self addMorphBack: aMorph.
