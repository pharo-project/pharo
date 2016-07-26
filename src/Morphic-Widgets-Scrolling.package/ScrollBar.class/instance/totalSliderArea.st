geometry
totalSliderArea
	| upperBoundsButton |
	upperBoundsButton := upButton.
	bounds isWide
		ifTrue: [
			upButton right > upperBoundsButton right
				ifTrue: [upperBoundsButton := upButton].
			^upperBoundsButton bounds topRight corner: downButton bounds bottomLeft]
		ifFalse:[
			upButton bottom > upperBoundsButton bottom
				ifTrue: [upperBoundsButton := upButton].
			^upperBoundsButton bounds bottomLeft corner: downButton bounds topRight].
