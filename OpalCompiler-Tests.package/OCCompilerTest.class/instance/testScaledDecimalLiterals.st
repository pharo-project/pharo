literals
testScaledDecimalLiterals
	"Equal ScaledDecimal with different scales should use different slots
	This is related to http://bugs.squeak.org/view.php?id=6797"
	
	"This correctly works when evaluated separately"
	self deny: (Smalltalk compiler evaluate: '0.5s1') scale = (Smalltalk compiler evaluate: '0.5s2') scale.
	
	"But not when evaluated together if literal reduction is too agressive"
	self deny: (Smalltalk compiler evaluate: '0.5s1 scale =  0.5s2 scale').