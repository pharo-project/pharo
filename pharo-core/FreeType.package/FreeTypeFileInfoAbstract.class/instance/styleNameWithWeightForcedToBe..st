accessing
styleNameWithWeightForcedToBe: aString
	| answer |
	answer := ''.
	stretch ifNotNil:[
		answer := answer ,stretch].
	answer := answer , ' ', aString.
	slant ifNotNil:[
		answer := answer , ' ', slant].
	answer := answer trimBoth.
	^answer
		 
	