running
createNil

	"Temporal nil: it is needed so the nil stub class can be initialized with it"
	objectSpace nilObject: (objectSpace objectMirrorOn: (UndefinedObject basicNew)).
	
	"We create the real nil and become it"
	objectSpace nilObject becomeInto: (self createStubForClassNamed: #UndefinedObject) basicNew.
	
	^ objectSpace nilObject