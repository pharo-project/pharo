testing
testProtocol
	"Test that we can access protocol correctly"
	"self debug: #testProtocol"

	| okCm notOkCm |
	okCm := Point>>#distanceTo: .
	self assert: (SourceFiles sourcedDataAt: okCm sourcePointer) =   
 'Point methodsFor: ''point functions'' stamp: ''StephaneDucasse 11/10/2015 18:24'''	.
	self assert: (SourceFiles protocolAt: okCm sourcePointer) = 'point functions'.
	
	notOkCm := Behavior >> #superclass.
	self assert: (SourceFiles protocolAt: notOkCm sourcePointer) =  'accessing class hierarchy'.
	