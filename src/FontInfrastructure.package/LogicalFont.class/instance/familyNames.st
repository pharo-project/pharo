accessing
familyNames
	"Answer an array containing the receiver's familyName
	followed by any fallbackFamilyNames"
	|answer|
	answer := {familyName}.
	fallbackFamilyNames ifNotNil:[
		answer := answer, fallbackFamilyNames].
	^answer