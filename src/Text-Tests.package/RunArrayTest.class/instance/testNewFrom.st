tests - instance creation
testNewFrom
	"self debug: #testNewFrom"
	| array |
	array := RunArray newFrom: #($a $b $b $b $b $c $c $a).
	self assert: array size = 8.
	self assert: array asArray = #($a $b $b $b $b $c $c $a).