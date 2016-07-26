testing
testTimeStamp
	"Test that we can access timeStamp correctly"
	"self debug: #testTimeStamp"

	| okCm notOkCm |
	okCm := Point>>#dist: .
	self assert: (SourceFiles sourcedDataAt: okCm sourcePointer) =   'Point methodsFor: ''point functions'' stamp: ''lr 7/4/2009 10:42'''.
	self assert: (SourceFiles timeStampAt: okCm sourcePointer) = 'lr 7/4/2009 10:42'.
	
	notOkCm := Behavior >> #supermostPrecodeCommentFor:.
	self assert: (SourceFiles timeStampAt: notOkCm sourcePointer) = ''.
	