tests
testTop
	"test the #top: messages and its consequences"

	| morph factor newTop newBounds |
	morph := Morph new.
	""
	factor := 10.
	newTop := self defaultTop + factor.
	newBounds := self defaultBounds translateBy: 0 @ factor.
	""
	morph top: newTop.
	""
	self assert: morph top = newTop;
		 assert: morph bounds = newBounds