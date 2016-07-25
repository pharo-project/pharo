tests - Float
testFloatGradualUnderflow
	"Gradual underflow are tricky.
	This is a non regression test for http://bugs.squeak.org/view.php?id=6976"

	| float trueFraction str |
	
	"as a preamble, use a base 16 representation to avoid round off error and check that number parsing is correct"
	trueFraction := 16r2D2593D58B4FC4 / (16 raisedTo: 256+13).
	"Parse the number in base 16 if possible - it is impossible if lowercase letter are allowed digits due to exponent letter ambiguity."
	float := self areLowercaseDigitsAllowed
		ifFalse: [NumberParser parse: '16r2.D2593D58B4FC4e-256']
		ifTrue: [trueFraction asFloat]..
	self assert: float asTrueFraction = trueFraction.
	self assert: float = trueFraction asFloat.

	"now print in base 10"
	str := (String new: 32) writeStream.
	float absPrintExactlyOn: str base: 10.
	
	"verify if SqNumberParser can read it back"
	self assert: (NumberParser parse: str contents) = float. 