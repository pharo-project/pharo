testing
testRevertOldMethod
	| definition changeRecord |
	Object compile: 'yourself ^ self' classified: '*MonticelloMocks'.
	definition := (RGMethodDefinition realClass: Object selector: #yourself) asMCMethodDefinition.
	changeRecord := definition overridenMethodOrNil.
	self assert: changeRecord notNil.
	self assert: changeRecord category = 'accessing'.
	changeRecord fileIn.