border-styles
drawWindowPlainDropShadowFor: aSystemWindow on: aCanvas
	"Draw the plain drop shadow for the given window."
	
	|blOffset brOffset trOffset|
	(aSystemWindow owner notNil and: [aSystemWindow owner isHandMorph])
		ifTrue: [^self]. "skip if being dragged"
	blOffset := (aSystemWindow wantsRoundedCorners and: [aSystemWindow roundedCorners includes: 2])	
		ifTrue: [7] ifFalse: [1].
	brOffset := (aSystemWindow wantsRoundedCorners and: [aSystemWindow roundedCorners includes: 3])	
		ifTrue: [7] ifFalse: [1].
	trOffset := (aSystemWindow wantsRoundedCorners and: [aSystemWindow roundedCorners includes: 4])	
		ifTrue: [7] ifFalse: [1].
	aCanvas
		fillRectangle: (aSystemWindow bounds bottomLeft + (blOffset @ 0)
						corner: aSystemWindow bounds bottomRight - (brOffset @ -1))
		fillStyle: aSystemWindow shadowColor;
		fillRectangle: (aSystemWindow bounds topRight + (0 @ trOffset)
						corner: aSystemWindow bounds bottomRight + (1 @ brOffset negated))
		fillStyle: aSystemWindow shadowColor.
	aCanvas
		clipBy: (aSystemWindow bounds bottomRight - brOffset
			corner: aSystemWindow bounds bottomRight + (1@1))
			during: [:c | ((RoundedBorder new
							baseColor: aSystemWindow shadowColor;
							width: 1) cornerRadius: brOffset - 1)
							frameRectangle: (aSystemWindow bounds translateBy: 1)
							on: c]