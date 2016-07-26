pool initialization
initializeDistanceCodes
	| dist |
	BaseDistance := WordArray new: MaxDistCodes.
	DistanceCodes := WordArray new: 512.
	dist := 0.
	1 to: 16 do:[:code|
		BaseDistance at: code put: dist.
		1 to: (1 bitShift: (ExtraDistanceBits at: code)) do:[:n|
			dist := dist + 1.
			DistanceCodes at: dist put: code-1]].
	dist = 256 ifFalse:[self error:'Whoops?!'].
	dist := dist >> 7.
	17 to: MaxDistCodes do:[:code|
		BaseDistance at: code put: dist << 7.
		1 to: (1 bitShift: (ExtraDistanceBits at: code)-7) do:[:n|
			dist := dist + 1.
			DistanceCodes at: 256 + dist put: code-1]].
