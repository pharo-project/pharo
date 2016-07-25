running
pragmasFor: exampleToTest
	^ pragmas 
		at: exampleToTest class 
		ifAbsentPut: [ Pragma allNamed: #gtInspectorPresentationOrder: from: exampleToTest class to: Object ]