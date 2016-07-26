*Athens-Morphic
drawSubmorphsOnAthensCanvas: anAthensCanvas
	| clip |
	clip := self clippingBounds intersect: anAthensCanvas clipRect.
	anAthensCanvas
		transformBy: self transform
		withClip: clip
		in: self innerBounds
		during: [ :c | submorphs do: [ :m | m fullDrawOnAthensCanvas: c ] ]