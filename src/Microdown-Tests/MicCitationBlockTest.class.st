Class {
	#name : #MicCitationBlockTest,
	#superclass : #TestCase,
	#category : #'Microdown-Tests-Extensions'
}

{ #category : #tests }
MicCitationBlockTest >> testCreateTheRightCitationObject [
	
	| doc |
	doc := MicroDownParser new parse: '?{citation|key=Duca99a}?'.
	self assert: doc children first children first class equals: MicCitationBlock.
	self assert: doc children first children first name equals: 'citation'.
	self assert: (doc children first children first arguments at: #key) equals: 'Duca99a'.
]

{ #category : #tests }
MicCitationBlockTest >> testDefaultingClassWhenNoTag [
	
	| doc |
	doc := MicroDownParser new parse: '?{citation2|key=Duca99a}?'.
	self assert: doc children first children first class equals: MicAnnotationBlock.
	self assert: doc children first children first name equals: 'citation2'.
	self assert: (doc children first children first arguments at: #key) equals: 'Duca99a'.
]
