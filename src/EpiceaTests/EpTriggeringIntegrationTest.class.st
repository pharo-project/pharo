Class {
	#name : #EpTriggeringIntegrationTest,
	#superclass : #EpEnabledIntegrationTest,
	#category : #'EpiceaTests-Integration'
}

{ #category : #accessing }
EpTriggeringIntegrationTest >> changesBrowserOpenedWindows [

	^ ChangesBrowser allSubInstances reject: [ :each | each window isClosed ]
]

{ #category : #accessing }
EpTriggeringIntegrationTest >> monticelloRepository [

	^ GoferResource current monticelloRepository
]

{ #category : #accessing }
EpTriggeringIntegrationTest >> resources [

	^ super resources copyWith: GoferResource
]

{ #category : #tests }
EpTriggeringIntegrationTest >> testMCVersionLoad [
	
	| gofer anEntry |
	gofer := Gofer new.
	gofer disablePackageCache.
	gofer 
		repository: self monticelloRepository;
		package: 'GoferFoo'.
	gofer load.
	gofer unload.

	"Check EpMonitor logged it correctly"
	self assert: (self countLogEventsWith: EpClassAddition) equals: 1.

	anEntry := (self allLogEntriesWith: EpClassAddition) anyOne.

	self 
		assert: (anEntry tags includesKey: EpLog triggererReferenceKey) 
		description: 'It did not recognize that the code change was triggered by other event.'
]

{ #category : #tests }
EpTriggeringIntegrationTest >> testNoTrigger [
	
	classFactory newClass.
	[ classFactory newClass ] asJob run.

	"Check EpMonitor logged them correctly"
	self assert: (self countLogEventsWith: EpClassAddition) equals: 2.

	(self allLogEntriesWith: EpClassAddition) do: [ :anEntry |
		self deny: (anEntry tags includesKey: EpLog triggererReferenceKey) ]
]
