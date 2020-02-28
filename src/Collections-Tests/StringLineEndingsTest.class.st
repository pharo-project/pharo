Class {
	#name : #StringLineEndingsTest,
	#superclass : #ParametrizedTestCase,
	#instVars : [
		'targetLineEnding',
		'originLineEnding',
		'testingSubjectWithTargetLineEnding',
		'testingSubjectWithOriginLineEnding',
		'templateString'
	],
	#category : #'Collections-Tests-Strings'
}

{ #category : #'building suites' }
StringLineEndingsTest class >> testParameters [
	^ ParametrizedTestMatrix new
		forSelector: #targetLineEnding addOptions: {String cr . String lf . String crlf};
		forSelector: #originLineEnding addOptions: {String cr . String lf . String crlf};
		forSelector: #templateString addOptions: {'I am a String{1}'. '{1}I am a String' . 'I am a{1}String'. '{1}I am{1}a String{1}'};
		
		
		
		yourself
		
	
 
]

{ #category : #running }
StringLineEndingsTest >> newTestingSubject [
	^ templateString format: {originLineEnding}
]

{ #category : #'as yet unclassified' }
StringLineEndingsTest >> originLineEnding: aString [ 
	originLineEnding := aString
]

{ #category : #running }
StringLineEndingsTest >> setUp [
	
	super setUp.
	testingSubjectWithOriginLineEnding := self newTestingSubject. 
	testingSubjectWithTargetLineEnding := testingSubjectWithOriginLineEnding withLineEndings: targetLineEnding.
]

{ #category : #'as yet unclassified' }
StringLineEndingsTest >> targetLineEnding: aString [ 
	targetLineEnding := aString
]

{ #category : #running }
StringLineEndingsTest >> templateString: aString [
	templateString := aString
]

{ #category : #'as yet unclassified' }
StringLineEndingsTest >> testLineEndinglessStringShouldBeEquals [
	| originWithoutLineEnding targetWithoutLineEnding |
	originWithoutLineEnding := testingSubjectWithOriginLineEnding
		copyReplaceAll: originLineEnding with: ''.
	targetWithoutLineEnding := testingSubjectWithTargetLineEnding
		copyReplaceAll: targetLineEnding with: ''.
	self assert: originWithoutLineEnding equals: targetWithoutLineEnding
]

{ #category : #'as yet unclassified' }
StringLineEndingsTest >> testOriginStringCanBeProducedFromTargetStringAndOriginalLineEnding [
	
	| testingSubjectWithTargetLineEndingTransformedWithOriginalLineEnding |
	testingSubjectWithTargetLineEndingTransformedWithOriginalLineEnding := testingSubjectWithTargetLineEnding withLineEndings: originLineEnding .
	self assert: testingSubjectWithTargetLineEndingTransformedWithOriginalLineEnding equals: testingSubjectWithOriginLineEnding 
	
	" transformedString := 'I'm a string <<lf>>' withLineEnding: <<cr>> 
	>>> assert: I'm a string <<cr>> (transformedString) equals: I'm a string <<cr>> ( subjectWithOriginLineEnding )"
]

{ #category : #tests }
StringLineEndingsTest >> testTargetLineEndingStringWithoutOriginLineEnding [
	self
		assert:
			((targetLineEnding includesSubstring: originLineEnding) 
				xor: [ (testingSubjectWithTargetLineEnding
						includesSubstring: originLineEnding) not ])
]
