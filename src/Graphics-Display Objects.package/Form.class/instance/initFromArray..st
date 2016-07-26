private
initFromArray: array
	"Fill the bitmap from array.  If the array is shorter,
	then cycle around in its contents until the bitmap is filled."
	| ax aSize array32 i j word16 |
	ax := 0.
	aSize := array size.
	aSize > bits size ifTrue:
		["backward compatibility with old 16-bit bitmaps and their forms"
		array32 := Array new: height * (width + 31 // 32).
		i := j := 0.
		1 to: height do:
			[:y | 1 to: width+15//16 do:
				[:x16 | word16 := array at: (i := i + 1).
				x16 odd ifTrue: [array32 at: (j := j+1) put: (word16 bitShift: 16)]
						ifFalse: [array32 at: j put: ((array32 at: j) bitOr: word16)]]].
		^ self initFromArray: array32].
	1 to: bits size do:
		[:index |
		(ax := ax + 1) > aSize ifTrue: [ax := 1].
		bits at: index put: (array at: ax)]