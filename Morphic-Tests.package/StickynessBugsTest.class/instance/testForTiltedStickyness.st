tests
testForTiltedStickyness
	"self new testForTiltedStickyness"

	"self run: #testForTiltedStickyness"

	| m |
	m := Morph new openCenteredInWorld.
	cases := Array with: m.	"save for tear down."
	self assert: m topRendererOrSelf isSticky not.
	m beSticky.
	self assert: m topRendererOrSelf isSticky.
	m addFlexShell.
	cases := Array with: m topRendererOrSelf.
	m topRendererOrSelf rotationDegrees: 45.0.
	self assert: m topRendererOrSelf isSticky.
	m beUnsticky.
	self assert: m topRendererOrSelf isSticky not.
	m topRendererOrSelf delete.
	^ true