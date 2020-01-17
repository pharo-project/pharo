"
Unions are data-types defining a single value that can be interpreted with different internal representations.
For example, the next piece of C code defines a type ==float_or_int== that can be seen as a float or as an int.

[[[language=c
typedef union {
  float as_float;
  int as_int;
} float_or_int;

float_or_int number;
number.as_float = 3.14f;

printf(""Integer representation of PI: %d\n"", number.as_int);
]]]

producing the next output:

[[[
Integer representation of PI: 1078523331
]]]

!!!! Defining an union using ==FFIUnion==

Unions are declared in uFFI as subclasses of the ==FFIUnion== class defining the same fields as defined in C, similarly as structures.
For example, defining our ==float_or_int== union is done as follows, defining a subclass of ==FFIUnion==, a ==fieldsDesc== class-side method returning the specification of the union fields, and finally sending the ==rebuildFieldAccessors== message to the union class we created.

[[[language=smalltalk
FFIUnion subclass: #FloatOrIntUnion
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'FFITutorial'

FloatOrIntUnion class >> fieldsDesc [
	^ #(
		float as_float;
		int as_int;
		)
]

FloatOrIntUnion rebuildFieldAccessors.
]]]

Doing this will automatically generate some boilerplate code to manipulate the values inside the union.
You will see that the union class gets redefined like structures did, containing some auto-generated accessors.

[[[language=smalltalk
FFIStructure subclass: #FloatOrIntUnion
	instanceVariableNames: ''
	classVariableNames: 'OFFSET_DENOMINATOR OFFSET_NUMERATOR'
	package: 'FFITutorial'

FloatOrIntUnion >> as_float [
	""This method was automatically generated""
	^handle floatAt: 1
]

FloatOrIntUnion >> as_float: anObject [
	""This method was automatically generated""
	handle floatAt: 1 put: anObject
]

FloatOrIntUnion >> as_int [
	""This method was automatically generated""
	^handle signedLongAt: 1
]

FloatOrIntUnion >> as_int: anObject [
	""This method was automatically generated""
	handle signedLongAt: 1 put: anObject
]
]]]

!!!! Using the defined union type

Once a union type is defined, we can allocate unions from it using the ==new== and ==externalNew== messages, that will allocate it in the Pharo heap or the external C heap respectively.

[[[language=smalltalk
""In Pharo heap""
aFloatOrInt := FloatOrIntUnion new.

""In C heap""
aFloatOrInt := FloatOrIntUnion externalNew.
]]]

We read or write in our union using the auto-generated accessors.

[[[language=smalltalk
foi as_float: 3.14.
foi as_int.
>>> 1078523331
]]]

And we can use it as an argument in a call\-out by using its type.

[[[language=smalltalk
FFITutorial >> firstByte: float_or_union [
  ^ self ffiCall: #(char float_or_int_first_byte(FloatOrIntUnion* float_or_union))
]
]]]

!!!! Implementation Details

I'm implemented as an extension of FFIStructure.
I put all of my fields at the same offset, which is zero.
"
Class {
	#name : #FFIUnion,
	#superclass : #FFIStructure,
	#category : #'UnifiedFFI-Objects'
}

{ #category : #private }
FFIUnion class >> compileFields: specArray withAccessors: aSymbol [
	| fieldSpec defineBoolean totalSize|
	defineBoolean := aSymbol = #always.
	fieldSpec := self fieldSpec.
	totalSize := 0.	
	externalStructureAlignment := 1.
	fieldSpec fieldsAndTypesDo: [ :fieldName :type | 
		(defineBoolean and: [ fieldName isSymbol ]) ifTrue: [ 
			self defineFieldAccessorsFor: fieldName startingAt: 1 type: type ]. 
		totalSize := totalSize max: type typeSize.
		externalStructureAlignment := externalStructureAlignment max: type typeAlignment  ].
	totalSize := totalSize alignedTo: externalStructureAlignment.
	
	"Real compiled spec is the compiled spec of fields plus a header with structure size and 
	 structure flag"
	"Temporal type to ensure cyclic (pointer) references will work (case of linked lists, 
	 for example). I do not like it, but it works :S"
	compiledSpec := {ExternalType pointerSpec}. 
	"Now I can reconsider it"
	compiledSpec := fieldSpec compileSpec copyWithFirst: (totalSize bitOr: FFIFlagStructure).
	ExternalType noticeModificationOf: self.
	^ compiledSpec
]

{ #category : #'register marshalling' }
FFIUnion class >> emitFlatStructureLayoutInto: flatStructureLayout [
	flatStructureLayout addMemoryFieldSize: self structureSize alignment: self structureAlignment
]
