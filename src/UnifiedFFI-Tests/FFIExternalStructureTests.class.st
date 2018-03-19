"
Tests for external structures
"
Class {
	#name : #FFIExternalStructureTests,
	#superclass : #TestCase,
	#category : #'UnifiedFFI-Tests-Tests'
}

{ #category : #tests }
FFIExternalStructureTests >> testExternalStructWithArray [
	| structWithArrays structureSize |	

	structureSize := FFITestArrayStructure structureSize.
	structWithArrays := FFITestArrayStructure externalNew autoRelease.
	self assert: structWithArrays getHandle isExternalAddress.
	structWithArrays getHandle 
		replaceFrom: 1 to: structureSize with: (ByteArray new: structureSize). 
		
	structWithArrays char5: (FFITypeArray fromHandle: 'ABCDE' asByteArray).
	structWithArrays byte10: (FFITypeArray fromHandle: #[1 2 3 4 5 6 7 8 9 0]).
	self 
		assert: structWithArrays char5 asArray 
		equals: {$A. $B. $C. $D. $E}.
	self 
		assert: structWithArrays byte10 asArray 
		equals: #(1 2 3 4 5 6 7 8 9 0).
]

{ #category : #tests }
FFIExternalStructureTests >> testExternallyAllocatedNestedStructure [
	| s1 |
	
	s1 := FFITestNestingStructure externalNew.
	self assert: s1 getHandle isExternalAddress.
	s1 nested byte: 42.	
	self assert: s1 nested byte = 42
]

{ #category : #tests }
FFIExternalStructureTests >> testExternallyAllocatedStructure [
	| struct struct2 |
	
	struct := FFITestStructure externalNew.
	self assert: struct getHandle isExternalAddress.

	self deny: struct isNull.
	[	struct byte: 10.
		struct short: -20.
		struct long: 100.
		struct float: 1.0.
		struct double: 2.0.
		"struct int64: 123456789101112."
	
		struct2 := FFITestStructure fromHandle: struct getHandle.
	
		self assert: (struct2 byte = 10).
		self assert: (struct2 short = -20).	
		self assert: (struct2 long = 100).	
		self assert: (struct2 float = 1.0).	
		self assert: (struct2 double = 2.0).	
		self flag: #todo. "This is not yet implemented"
		"self assert: (struct2 int64 = 123456789101112)."	 ] 
	ensure: [  struct free ]
]

{ #category : #tests }
FFIExternalStructureTests >> testFlatStructureLayout [
	| flatLayout |
	flatLayout := FFITestStructureSmallFloatStructure flatStructureLayout.
	self assert: flatLayout size equals: 16.
	self assert: flatLayout alignment equals: 4.
	self assert: flatLayout fields size equals: 4.
	self assert: flatLayout fields first registerClass equals: #float.
	self assert: flatLayout fields second registerClass equals: #float.
	self assert: flatLayout fields third registerClass equals: #float.
	self assert: flatLayout fields fourth registerClass equals: #float.

]

{ #category : #tests }
FFIExternalStructureTests >> testFlatStructureLayout2 [
	| flatLayout |
	flatLayout := FFITestStructureSmallIntFloatStructure flatStructureLayout.
	self assert: flatLayout size equals: 16.
	self assert: flatLayout alignment equals: 4.
	self assert: flatLayout fields size equals: 4.
	self assert: flatLayout fields first registerClass equals: #float.
	self assert: flatLayout fields second registerClass equals: #integer.
	self assert: flatLayout fields third registerClass equals: #float.
	self assert: flatLayout fields fourth registerClass equals: #float.

]

{ #category : #tests }
FFIExternalStructureTests >> testFlatStructureLayoutSysVAMD64PostProcess [
	| flatLayout |
	flatLayout := FFITestStructureSmallFloatStructure flatStructureLayout sysVAMD64PostProcess.
	self assert: flatLayout size equals: 16.
	self assert: flatLayout alignment equals: 4.
	self assert: flatLayout fields size equals: 2.
	self assert: flatLayout fields first registerClass equals: #float.
	self assert: flatLayout fields second registerClass equals: #float.
	self assert: flatLayout integerRegisterCount equals: 0.
	self assert: flatLayout floatRegisterCount equals: 2.
]

{ #category : #tests }
FFIExternalStructureTests >> testFlatStructureLayoutSysVAMD64PostProcess2 [
	| flatLayout |
	flatLayout := FFITestStructureSmallIntFloatStructure flatStructureLayout sysVAMD64PostProcess.
	self assert: flatLayout size equals: 16.
	self assert: flatLayout alignment equals: 4.
	self assert: flatLayout fields size equals: 2.
	self assert: flatLayout fields first registerClass equals: #integer.
	self assert: flatLayout fields second registerClass equals: #float.
	self assert: flatLayout integerRegisterCount equals: 1.
	self assert: flatLayout floatRegisterCount equals: 1.
	
]

{ #category : #tests }
FFIExternalStructureTests >> testNestedStructure [
	| s1 |

	s1 := FFITestNestingStructure new.
	s1 nested byte: 42.
	s1 nested float: 4.0.
	s1 nested double: 42.0.

	s1 nested long: -123456.
	s1 nested ulong: 123456.

	self assert: s1 nested byte equals: 42.	
	self assert: s1 nested float equals: 4.0.	
	self assert: s1 nested double equals: 42.0.	
	self assert: s1 nested long equals: -123456.
	self assert: s1 nested ulong equals: 123456.

]

{ #category : #tests }
FFIExternalStructureTests >> testNestedStructureWithArray [
	| nestedStruct |	

	nestedStruct := FFITestNestingStructureWithArray new.
	self deny: nestedStruct nested getHandle isExternalAddress.
	self 
		assert: nestedStruct nested char5 asArray
		equals: {Character null. Character null. Character null. Character null. Character null }.
	self 
		assert: nestedStruct nested byte10 asArray 
		equals: #(0 0 0 0 0 0 0 0 0 0).
		
	nestedStruct nested char5: (FFITypeArray fromHandle: 'ABCDE' asByteArray).
	nestedStruct nested byte10: (FFITypeArray fromHandle: #[1 2 3 4 5 6 7 8 9 0]).
	self 
		assert: nestedStruct nested char5 asArray 
		equals: {$A. $B. $C. $D. $E}.
	self 
		assert: nestedStruct nested byte10 asArray 
		equals: #(1 2 3 4 5 6 7 8 9 0).
]

{ #category : #tests }
FFIExternalStructureTests >> testStructAccess [

	| struct |
	
	struct := FFITestStructure new.
	self deny: struct getHandle class = ExternalAddress.	
	self deny: struct isNull.
	
	struct byte: 10.
	struct short: -20.
	struct long: 100.
	struct float: 1.0.
	struct double: 2.0.
		self flag: #todo. "This is not yet implemented"
	"struct int64: 123456789101112."
	
	self assert: (struct byte = 10).
	self assert: (struct short = -20).	
	self assert: (struct long = 100).	
	self assert: (struct float = 1.0).	
	self assert: (struct double = 2.0).	
	"self assert: (struct int64 = 123456789101112).	"
	
]

{ #category : #tests }
FFIExternalStructureTests >> testStructWithArray [
	| structWithArrays |	

	structWithArrays := FFITestArrayStructure new.
	self deny: structWithArrays getHandle isExternalAddress.
	self 
		assert: structWithArrays char5 asArray
		equals: {Character null. Character null. Character null. Character null. Character null }.
	self 
		assert: structWithArrays byte10 asArray 
		equals: #(0 0 0 0 0 0 0 0 0 0).
		
	structWithArrays char5: (FFITypeArray fromHandle: 'ABCDE' asByteArray).
	structWithArrays byte10: (FFITypeArray fromHandle: #[1 2 3 4 5 6 7 8 9 0]).
	self 
		assert: structWithArrays char5 asArray 
		equals: {$A. $B. $C. $D. $E}.
	self 
		assert: structWithArrays byte10 asArray 
		equals: #(1 2 3 4 5 6 7 8 9 0).
	
	

]

{ #category : #tests }
FFIExternalStructureTests >> testStructWithPointerAccess [
	| s1 s2 |	

	s1 := FFITestPointerStructure new.
	self assert: s1 getHandle isExternalAddress not.
	self assert: s1 pointer isNull.
	self assert: s1 nestedPointer isNull.
	
	s2 := FFITestStructure externalNew.
	s2 long: 42.
	s1 nestedPointer: s2.
	self deny: s1 nestedPointer isNull.
	self assert: s1 nestedPointer long = 42.
]
