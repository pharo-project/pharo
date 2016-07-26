tests
testValidatePorts
	presentation selection: #($e $f).
	self assert: presentation selection equals: #($e $f).
	presentation selection: $a.
	self assert: presentation selection equals: $a.
	presentation selection: 'wrongSelection'.	"this should be invalid"
	self assert: presentation selection equals: $a