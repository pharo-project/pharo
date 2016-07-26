testing
testBOMWrite
	
	"Test that BOM is written correctly for both endians, both when using a nextPut:, and nextPutAll:"
	
	| batchConversion oneByOneConversion |
	batchConversion := [:converter :stream | converter nextPutAll: 'test' toStream: stream].
	oneByOneConversion := [:converter :stream | 'test' do: [:each | converter nextPut: each toStream: stream]].
	
	self assertBOMIsWrittenWithOrder: #bigEndian 
		andConversion: oneByOneConversion.
	self assertBOMIsWrittenWithOrder: #bigEndian 
		andConversion: batchConversion.
		
	self assertBOMIsWrittenWithOrder: #littleEndian 
		andConversion: oneByOneConversion.
	self assertBOMIsWrittenWithOrder: #littleEndian 
		andConversion: batchConversion.
		
	self assertBOMIsWrittenWithOrder: nil 
		andConversion: oneByOneConversion.
	self assertBOMIsWrittenWithOrder: nil 
		andConversion: batchConversion.