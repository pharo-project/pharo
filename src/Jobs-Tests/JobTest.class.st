"
SUnit tests for Job class
"
Class {
	#name : #JobTest,
	#superclass : #AbstractEnvironmentTestCase,
	#category : #'Jobs-Tests-Base'
}

{ #category : #running }
JobTest >> tearDown [

	testingEnvironment at: #SystemProgressMorph ifPresent: [:spm | 
		spm uniqueInstance bars do: [ :e | e close ]].
	super tearDown
]

{ #category : #tests }
JobTest >> testChildJob [
	| wasRun |
	wasRun := false.

	[ :job | 
	[ :job2 | 
	self assert: job children size equals: 1.
	self assert: job children first equals: job2.
	wasRun := true ] asJob run.

	self assertEmpty: job children ] asJob run.

	self assert: wasRun
]

{ #category : #tests }
JobTest >> testCurrentJob [
	[ :job | 
	[ :job2 | 
	self assert: Job current identicalTo: job2.
	self deny: Job current identicalTo: job ] asJob run.

	self assert: Job current identicalTo: job ] asJob run
]

{ #category : #tests }
JobTest >> testJobAnnouncements [
	| announcements |
	announcements := OrderedCollection new.

	Job jobAnnouncer weak when: JobStart send: #add: to: announcements.
	Job jobAnnouncer weak when: JobEnd send: #add: to: announcements.
	Job jobAnnouncer weak when: JobChange send: #add: to: announcements.

	self assertEmpty: announcements.

	[ :job | job currentValue: 1 ] asJob run.

	self assert: (announcements collect: #class as: Array) equals: {JobStart . JobChange . JobEnd}.

	Job jobAnnouncer unsubscribe: announcements
]

{ #category : #tests }
JobTest >> testLookupJob [
	[ :job | 
	[ :job2 | 
	| noneFound result |
	noneFound := false.
	result := job2 lookup: [ :aJob | aJob == job2 ] ifNone: [ noneFound := true ].
	self assert: result identicalTo: job2.
	self deny: noneFound.

	result := job2 lookup: [ :aJob | aJob == job ] ifNone: [ noneFound := true ].
	self assert: result identicalTo: job.
	self deny: noneFound.

	result := job
		lookup: [ :aJob | aJob == job2 ]
		ifNone: [ noneFound := true.
			42 ].
	self assert: result equals: 42.
	self assert: noneFound ] asJob run.

	self assert: Job current identicalTo: job ] asJob run
]

{ #category : #tests }
JobTest >> testOwner [
	[ :job | 
	self assert: job owner identicalTo: self.
	[ :job2 | 
	self assert: job owner identicalTo: self.
	self assert: job2 owner identicalTo: self ] asJob run ] asJob run
]

{ #category : #tests }
JobTest >> testProgress [
	| job |
	job := Job new.
	self assert: job progress equals: 0.0.
	job min: 0; max: 1.
	self assert: job progress equals: 0.0.
	job currentValue: 1.
	self assert: job progress equals: 1.0
]

{ #category : #tests }
JobTest >> testProgressChangeByCurrentValue [
	| job |
	job := Job new.
	job min: 1; max: 11.
	self assert: job progress equals: 0.0.
	job currentValue: 6.
	self assert: job progress equals: 0.5.
	job currentValue: 11.
	self assert: job progress equals: 1.0
]

{ #category : #tests }
JobTest >> testSingleJob [
	| wasRun |
	wasRun := false.
	[ :job | 
	self assertEmpty: job children.
	wasRun := true ] asJob run.
	self assert: wasRun
]
