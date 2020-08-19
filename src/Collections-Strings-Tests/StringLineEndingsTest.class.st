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
	#category : #'Collections-Strings-Tests-Base'
}

{ #category : #'building suites' }
StringLineEndingsTest class >> testParameters [
	^ ParametrizedTestMatrix new
		forSelector: #targetLineEnding
			addOptions:
			{String cr.
			String lf.
			String crlf};
		forSelector: #originLineEnding
			addOptions:
			{String cr.
			String lf.
			String crlf};
		forSelector: #templateString
			addOptions:
			{'I am a String{1}' . '{1}I am a String' . 'I am a{1}String'.
			'{1}I am{1}a String{1}'};
		yourself
]

{ #category : #running }
StringLineEndingsTest >> newTestingSubject [
	^ templateString format: {originLineEnding}
]

{ #category : #accessing }
StringLineEndingsTest >> originLineEnding: aString [ 
	originLineEnding := aString
]

{ #category : #running }
StringLineEndingsTest >> setUp [
	
	super setUp.
	testingSubjectWithOriginLineEnding := self newTestingSubject. 
	testingSubjectWithTargetLineEnding := testingSubjectWithOriginLineEnding withLineEndings: targetLineEnding.
]

{ #category : #accessing }
StringLineEndingsTest >> targetLineEnding: aString [ 
	targetLineEnding := aString
]

{ #category : #running }
StringLineEndingsTest >> templateString: aString [
	templateString := aString
]

{ #category : #tests }
StringLineEndingsTest >> testLineEndinglessStringShouldBeEquals [
	| originWithoutLineEnding targetWithoutLineEnding |
	originWithoutLineEnding := testingSubjectWithOriginLineEnding
		copyReplaceAll: originLineEnding with: ''.
	targetWithoutLineEnding := testingSubjectWithTargetLineEnding
		copyReplaceAll: targetLineEnding with: ''.
	self assert: originWithoutLineEnding equals: targetWithoutLineEnding
]

{ #category : #tests }
StringLineEndingsTest >> testOriginStringCanBeProducedFromTargetStringAndOriginalLineEnding [
	
	" This test is for testing that we can tranform a string with certain line ending to a another line ending, and then transform it back to the original line ending. "
	
	| testingSubjectWithTargetLineEndingTransformedWithOriginalLineEnding |
	testingSubjectWithTargetLineEndingTransformedWithOriginalLineEnding := testingSubjectWithTargetLineEnding withLineEndings: originLineEnding .
	self assert: testingSubjectWithTargetLineEndingTransformedWithOriginalLineEnding equals: testingSubjectWithOriginLineEnding 
	
	
]

{ #category : #tests }
StringLineEndingsTest >> testTargetLineEndingStringWithoutOriginLineEnding [
	self
		assert:
			((targetLineEnding includesSubstring: originLineEnding) 
				xor: [ (testingSubjectWithTargetLineEnding
						includesSubstring: originLineEnding) not ])
]
