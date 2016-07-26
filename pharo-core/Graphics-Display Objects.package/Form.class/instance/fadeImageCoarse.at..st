transitions
fadeImageCoarse: otherImage at: topLeft
	"Display fadeImageCoarse: (Form fromDisplay: (40@40 extent: 300@300)) reverse at: 40@40"
	| d pix|
	d := self depth.
	^ self fadeImage: otherImage at: topLeft indexAndMaskDo:
		[:i :mask | | j |
		i=1 ifTrue: [pix := (1 bitShift: d) - 1.
					1 to: 8//d-1 do: [:q | pix := pix bitOr: (pix bitShift: d*4)]]			.
		i <= 16 ifTrue:[
		j := i-1//4+1.
		(0 to: 28 by: 4) do: [:k |
			mask bits at: j+k
				put: (pix bitOr: (mask bits at: j+k))].
		true]
		ifFalse: [false]]