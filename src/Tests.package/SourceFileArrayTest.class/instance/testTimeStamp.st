testing
testTimeStamp
	"Test that we can access timeStamp correctly"
	"self debug: #testTimeStamp"

	| okCm notOkCm |
	okCm := Point>>#distanceTo: .
	self assert: (SourceFiles sourcedDataAt: okCm sourcePointer) = 'Point methodsFor: ''point functions'' stamp: ''StephaneDucasse 11/10/2015 18:24'''.
	self assert: (SourceFiles timeStampAt: okCm sourcePointer) =
 'StephaneDucasse 11/10/2015 18:24'.	
	notOkCm := Behavior >> #supermostPrecodeCommentFor:.
	self assert: (SourceFiles timeStampAt: notOkCm sourcePointer) = ''.
	