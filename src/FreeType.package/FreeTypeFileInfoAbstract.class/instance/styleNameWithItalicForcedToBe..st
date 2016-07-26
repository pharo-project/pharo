accessing
styleNameWithItalicForcedToBe: aString
	| answer |
	answer := ''.
	stretch ifNotNil:[
		answer := answer ,stretch].
	(weight notNil "and:[weight asLowercase ~= 'medium']") 
		ifTrue:[
			answer := answer , ' ', weight].
	answer := answer , ' ', aString.
	answer := answer trimBoth.
	^answer
		 
	