tests
testParseFieldsStructure
	| fieldSpec |
	
	fieldSpec := self newParser   
		parseFields: FFITestStructure fields 
		structure: FFITestStructure.
		
	self assert: fieldSpec notNil. 
	self assert: fieldSpec fieldNames = #(byte short long float double int64).
	self assert: (fieldSpec typeFor: #byte) class = FFIUInt8.
	self assert: (fieldSpec typeFor: #short) class = FFIInt16.
	self assert: (fieldSpec typeFor: #long) class = FFIInt32.
	self assert: (fieldSpec typeFor: #float) class = FFIFloat32.
	self assert: (fieldSpec typeFor: #double) class = FFIFloat64.
	self assert: (fieldSpec typeFor: #int64) class = FFIInt64.
	