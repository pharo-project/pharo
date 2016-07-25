display
scaleFor:  viewedMorphBox in: myBox
	"Compute the proper scale for the thumbnail."

	|   scale  scaleX scaleY ratio factor  |
scaleX := myBox width asFloat / viewedMorphBox width.
				scaleY := myBox height asFloat / viewedMorphBox height.
				ratio := scaleX / scaleY.
				factor := 1.0 / EccentricityThreshhold.
				ratio < factor
					ifTrue:
						[scale := (scaleX) @ (factor * scaleY)]
					ifFalse:
						[ratio > EccentricityThreshhold
							ifTrue:
								[scale := (factor * scaleX) @ scaleY]
							ifFalse:
								[scale := scaleX min: scaleY]].
^ scale
