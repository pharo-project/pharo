Class {
	#name : #HEExportedLiteralTest,
	#superclass : #HEAbstractBinaryClassBuilderTest,
	#category : 'Hermes-Tests'
}

{ #category : #tests }
HEExportedLiteralTest >> doTestExportLiteral: original [ 
	| exported |
	exported := self export: original.
	self assert: exported equals: original.
]

{ #category : #tests }
HEExportedLiteralTest >> export: original [
	original asExportedLiteral writeInto: writer.
	writer flush.
	
	^ (HEExportedLiteral readFrom: reader) value.	

]

{ #category : #tests }
HEExportedLiteralTest >> testFraction [
	self doTestExportLiteral: 1/3.
	self doTestExportLiteral: 0.5s3.	
]

{ #category : #tests }
HEExportedLiteralTest >> testLiteralEqualsOfFloat [
	self assert: ((self export:0.0) literalEqual: 0.0).
	self assert: ((self export:-0.0) literalEqual: 0.0 negated).

	self deny: ((self export:0.0) literalEqual: 0.0 negated).
	self deny: ((self export:-0.0) literalEqual: -0.0 negated).
	self deny: ((self export:0.0) literalEqual: -0.0).


	self assert: ((self export:3.0) literalEqual: 3.0).
	self assert: ((self export:-2.5) literalEqual: -2.5).

]

{ #category : #tests }
HEExportedLiteralTest >> testTagsAreOk [
	HEExportedLiteral checkAllTags.
	HEExportedLiteral nextTag.
]

{ #category : #tests }
HEExportedLiteralTest >> testWideString [
	self doTestExportLiteral: '1796exportFixes-tkMX' asWideString.
	self doTestExportLiteral: (Character value: 257) asString.

]
