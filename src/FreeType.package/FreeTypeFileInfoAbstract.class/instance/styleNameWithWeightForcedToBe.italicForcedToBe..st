accessing
styleNameWithWeightForcedToBe: aString italicForcedToBe: aString2
	| answer |
	answer := ''.
	stretch ifNotNil:[
		answer := answer ,stretch].
	answer := answer , ' ', aString.
	answer := answer , ' ', aString2.
	answer := answer trimBoth.
	^answer
		 
	