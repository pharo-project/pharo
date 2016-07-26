instance creation
dotOfSize: diameter
	"Create a form which contains a round black dot."
	| radius form bb rect centerX centerY centerYBias centerXBias radiusSquared xOverY maxy dx |
	radius := diameter//2.
	form := self extent: diameter@diameter offset: (0@0) - (radius@radius).	
	bb := (BitBlt toForm: form)
		sourceX: 0; sourceY: 0;
		combinationRule: Form over;
		fillColor: Color black.
	rect := form boundingBox.
	centerX := rect center x.
	centerY := rect center y.
	centerYBias := rect height odd ifTrue: [0] ifFalse: [1].
	centerXBias := rect width odd ifTrue: [0] ifFalse: [1].
	radiusSquared := (rect height asFloat / 2.0) squared - 0.01.
	xOverY := rect width asFloat / rect height asFloat.
	maxy := rect height - 1 // 2.

	"First do the inner fill, and collect x values"
	0 to: maxy do:
		[:dy |
		dx := ((radiusSquared - (dy * dy) asFloat) sqrt * xOverY) truncated.
		bb	destX: centerX - centerXBias - dx
			destY: centerY - centerYBias - dy
			width: dx + dx + centerXBias + 1
			height: 1;
			copyBits.
		bb	destY: centerY + dy;
			copyBits].
	^ form
"
Time millisecondsToRun:
	[1 to: 20 do: [:i | (Form dotOfSize: i) displayAt: (i*20)@(i*20)]]
"