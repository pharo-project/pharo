testing
testTimeStamp
	"Test that we can access timeStamp correctly"
	"self debug: #testTimeStamp"

	| okCm |
	okCm := Point>>#distanceTo: .
	self assert: (((SourceFiles sourcedDataAt: okCm sourcePointer)) beginsWith:   
 'Point methodsFor: ''point functions'' stamp: ').
	self assert: (SourceFiles timeStampAt: okCm sourcePointer) notEmpty.	

	