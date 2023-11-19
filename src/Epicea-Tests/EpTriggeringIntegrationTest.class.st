Class {
	#name : 'EpTriggeringIntegrationTest',
	#superclass : 'EpEnabledIntegrationTest',
	#category : 'Epicea-Tests-Integration',
	#package : 'Epicea-Tests',
	#tag : 'Integration'
}

{ #category : 'tests' }
EpTriggeringIntegrationTest >> testNoTrigger [

	classFactory newClass.
	[ classFactory newClass ] asJob run.

	"Check EpMonitor logged them correctly"
	self assert: (self countLogEventsWith: EpClassAddition) equals: 2.

	(self allLogEntriesWith: EpClassAddition) do: [ :anEntry | self deny: (anEntry tags includesKey: EpLog triggererReferenceKey) ]
]
