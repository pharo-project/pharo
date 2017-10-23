tests-categorization
testNonCharacterPositive
	| nonCps |
	nonCps := (16rFDD0 to: 16rFDEF) asSet. 
	nonCps addAll: (16r0FFFE to: 16r10FFFE by: 16r10000).
	nonCps addAll: (16r0FFFF to: 16r10FFFF by: 16r10000).
	self assert: nonCps size equals: 66.		"defined that way by the standard"
	nonCps do: [ :each | self assert: (Unicode isNonCharacter: (Character codePoint: each)) ]