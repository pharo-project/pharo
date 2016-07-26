private
scaledAlphaMix: theta of: lastWord with: nextWord
	"Open-coded version of alpha mixing two 32bit pixel words and returning the scaled pixel value."
	| word0 word1 a0 a1 alpha v0 v1 vv value |
	word0 := lastWord.
	word1 := nextWord.
	"note: extract alpha first so we'll be in SmallInteger range afterwards"
	a0 := word0 bitShift: -24. a1 := word1 bitShift: -24.
	alpha := a0 + (a1 - a0 * theta) truncated.
	"Now make word0 and word1 SmallIntegers"
	word0 := word0 bitAnd: 16rFFFFFF. word1 := word1 bitAnd: 16rFFFFFF.
	"Compute first component value"
	v0 := (word0 bitAnd: 255). v1 := (word1 bitAnd: 255).
	vv := (v0 + (v1 - v0 * theta) truncated) * alpha // 255.
	value := vv.
	"Compute second component value"
	v0 := ((word0 bitShift: -8) bitAnd: 255). v1 := ((word1 bitShift: -8) bitAnd: 255).
	vv := (v0 + (v1 - v0 * theta) truncated) * alpha // 255.
	value := value bitOr: (vv bitShift: 8).
	"Compute third component value"
	v0 := ((word0 bitShift: -16) bitAnd: 255). v1 := ((word1 bitShift: -16) bitAnd: 255).
	vv := (v0 + (v1 - v0 * theta) truncated) * alpha // 255.
	value := value bitOr: (vv bitShift: 16).
	"Return result"
	^value bitOr: (alpha bitShift: 24)