initialization
initializeTranslucentPatterns
	
	TranslucentPatterns := Array new: 8. 
	#(1 2 4 8 ) do: 
		[ :d | | mask bits pattern patternList | 
		patternList := Array new: 5.
		mask := (1 bitShift: d) - 1.
		bits := 2 * d.
		[ bits >= 32 ] whileFalse: 
			[ mask := mask bitOr: (mask bitShift: bits).	"double the length of mask"
			bits := bits + bits ].
		"0% pattern"
		pattern := Bitmap 
			with: 0
			with: 0.
		patternList 
			at: 1
			put: pattern.
		"25% pattern"
		pattern := Bitmap 
			with: mask
			with: 0.
		patternList 
			at: 2
			put: pattern.
		"50% pattern"
		pattern := Bitmap 
			with: mask
			with: mask bitInvert32.
		patternList 
			at: 3
			put: pattern.
		"75% pattern"
		pattern := Bitmap 
			with: mask
			with: 4294967295.
		patternList 
			at: 4
			put: pattern.
		"100% pattern"
		pattern := Bitmap 
			with: 4294967295
			with: 4294967295.
		patternList 
			at: 5
			put: pattern.
		TranslucentPatterns 
			at: d
			put: patternList ]