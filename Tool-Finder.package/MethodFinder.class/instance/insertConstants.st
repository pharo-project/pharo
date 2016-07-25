search
insertConstants
	"see if one of several known expressions will do it. C is the constant we discover here."
	"C  data1+C  data1*C  data1//C  (data1*C1 + C2) (data1 = C) (data1 ~= C) (data1 <= C) (data1 >= C) 
 (data1 mod C)"

	thisData size >= 2 ifFalse: [^ false].	"need 2 examples"
	(thisData at: 1) size = 1 ifFalse: [^ false].	"only one arg, data1"

	self const ifTrue: [^ true].
	self constUsingData1Value ifTrue: [^ true].
		"(data1 ?? const), where const is one of the values of data1"
		" == ~~ ~= = <= >= "

	self allNumbers ifFalse: [^ false].
	self constMod ifTrue: [^ true].
	self constPlus ifTrue: [^ true].
	self constMult ifTrue: [^ true].
	self constDiv ifTrue: [^ true].
	self constLinear ifTrue: [^ true].
	^ false