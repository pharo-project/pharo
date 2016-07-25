testing
testPrintingClassSide
	"self run: #testPrintingClassSide"
	
	self assertPrints: self t6 classSide definition
		like: 'T6 classTrait
	uses: T1 classTrait + T2 classTrait'