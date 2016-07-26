private
absoluteSquared8Dot24: value
	"Compute the squared value of a 8.24 number with 0.0 <= value < 1.0,
	e.g., compute (value * value) bitShift: -24"
	| halfWord1 halfWord2 result |
	(value >= 0 and:[value < 16r1000000]) ifFalse:[^self error:'Value out of range'].
	halfWord1 := value bitAnd: 16rFFFF.
	halfWord2 := (value bitShift: -16) bitAnd: 255.

	result := (halfWord1 * halfWord1) bitShift: -16. "We don't need the lower 16bits at all"
	result := result + ((halfWord1 * halfWord2) * 2).
	result := result + ((halfWord2 * halfWord2) bitShift: 16).
	"word1 := halfWord1 * halfWord1.
	word2 := (halfWord2 * halfWord1) + (word1 bitShift: -16).
	word1 := word1 bitAnd: 16rFFFF.
	word2 := word2 + (halfWord1 * halfWord2).
	word2 := word2 + ((halfWord2 * halfWord2) bitShift: 16)."

	^result bitShift: -8