Class {
	#name : #PRDocumentSampleTest,
	#superclass : #TestCase,
	#category : #'Pillar-ExporterRichText'
}

{ #category : #tests }
PRDocumentSampleTest >> testHeaderLevel1Exist [

	| sample |
	sample := PRDocumentSample new headerLevel1.
	self assert: sample hasChildren.
]
