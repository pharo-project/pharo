testing
testFileOutNonAscii
	"My test class, class side, contains a method with non-ascii characters, 
	fileOut used to fail due to #backChunk problems"
	
	(self fileOutTestClass name asString, '.st') asFileReference ensureDelete.
	self fileOutTestClass fileOut