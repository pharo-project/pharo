converting
asSmallAngleDegrees
	"Return the receiver normalized to lie within the range (-180, 180)"

	| pos |
	pos := self \\ 360.
	pos > 180 ifTrue: [pos := pos - 360].
	^ pos

"#(-500 -300 -150 -5 0 5 150 300 500 1200) collect: [:n | n asSmallAngleDegrees]"