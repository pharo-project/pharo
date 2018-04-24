Class {
	#name : #GTSpotterStepTest,
	#superclass : #TestCase,
	#instVars : [
		'spotter',
		'currentStep',
		'previousStep',
		'currentProcessor',
		'currentCandidate',
		'currentContext',
		'previousContext'
	],
	#category : #'GT-Tests-Spotter-Scripting'
}

{ #category : #convenience }
GTSpotterStepTest >> assertCandidateMatching: aBlock [
	| candidates |
	self assert: currentProcessor notNil.
	
	candidates := currentProcessor allFilteredCandidates select: [ :each | aBlock value: each ].
	
	self deny: candidates isEmpty.
	self assert: candidates size = 1.
	
	currentCandidate := candidates anyOne.
	self assert: currentCandidate notNil.
	
	^ currentCandidate
]

{ #category : #convenience }
GTSpotterStepTest >> assertDiveIn [
	self assertDiveIn: #diveIn
]

{ #category : #private }
GTSpotterStepTest >> assertDiveIn: aSelector [
	| previousStepsSize link |
	" backup previous conditions "
	previousStepsSize := spotter steps size.
	previousStep := spotter currentStep.

	" get and validate the link "
	link := spotter currentStep candidates getCandidateLink: currentCandidate in: currentProcessor.
	
	self assert: link notNil.
	self assert: link candidate == currentCandidate.
	self assert: link processor == currentProcessor.

	" set the selection - like the UI would do "
	self shouldnt: [ spotter currentStep select: link ] raise: Error.
	self assert: spotter currentStep selected == link.
	
	" perform the dive operation - like the UI would do "
	self shouldnt: [ spotter currentStep perform: aSelector ] raise: Error.
	currentStep := spotter currentStep.
	currentProcessor := currentCandidate := previousContext := currentContext := nil.
	
	self deny: spotter steps isEmpty .
	self assert: spotter steps size > previousStepsSize.
	self assert: spotter steps size = (previousStepsSize + 1).
	
	self deny: previousStep = currentStep.
	self deny: previousStep = spotter currentStep.
	self assert: currentStep = spotter currentStep.
]

{ #category : #convenience }
GTSpotterStepTest >> assertDiveInCategory [
	self assertDiveIn: #diveInCategory
]

{ #category : #convenience }
GTSpotterStepTest >> assertDiveOut [
	| previousStepsSize |
	" backup previous conditions "
	previousStepsSize := spotter steps size.
	previousStep := spotter currentStep.

	" perform the dive operation - like the UI would do "
	self shouldnt: [ spotter popStep ] raise: Error.
	currentStep := spotter currentStep.
	currentProcessor := currentCandidate := previousContext := currentContext := nil.
	
	self assert: spotter steps size < previousStepsSize.
	self assert: spotter steps size = (previousStepsSize - 1).
	
	self deny: (previousStepsSize > 1 and: [ previousStep = currentStep ]).
	self deny: (previousStepsSize > 1 and: [ previousStep = spotter currentStep ]).
	self assert: currentStep = spotter currentStep.
]

{ #category : #convenience }
GTSpotterStepTest >> assertProcessorByTitle: aString [
	^ self assertProcessorMatching: [ :processor | processor title = aString ]
]

{ #category : #convenience }
GTSpotterStepTest >> assertProcessorIncludes: anObject [
	self assert: anObject notNil.
	self assert: currentProcessor notNil.
	self assert: currentProcessor allFilteredCandidates notEmpty.
	
	self assert: (currentProcessor allFilteredCandidates anySatisfy: [ :candidate | candidate = anObject ])
]

{ #category : #convenience }
GTSpotterStepTest >> assertProcessorIncludesAll: aCollection [
	self assert: aCollection notEmpty.
	self assert: currentProcessor notNil.
	self assert: currentProcessor allFilteredCandidates notEmpty.
	
	aCollection do: [ :each | 
		self assert: (currentProcessor allFilteredCandidates anySatisfy: [ :candidate | 
			candidate = each ]) ]
]

{ #category : #convenience }
GTSpotterStepTest >> assertProcessorMatching: aBlock [
	| processors |
	processors := spotter currentStep processors select: [ :each | aBlock value: each ].
	
	self assert: processors notEmpty.
	self assert: processors size = 1.
	
	currentProcessor := processors anyOne.
	self assert: currentProcessor notNil.
	
	^ currentProcessor
]

{ #category : #convenience }
GTSpotterStepTest >> assertProcessorNotEmpty [
	self assert: currentProcessor notNil.
	self deny: currentProcessor allFilteredCandidates isEmpty.
]

{ #category : #convenience }
GTSpotterStepTest >> assertSearch: aString [

	" assert conditions before search "
	self deny: (spotter currentStep context matches: aString).
	
	self basicSearch: aString.
		
	" assert conditions after search "
	self deny: previousContext == currentContext.
	self deny: previousContext == spotter currentStep context.
	self assert: spotter currentStep context text asString = aString asString
]

{ #category : #convenience }
GTSpotterStepTest >> assortCandidatesSize: aNumber [
]

{ #category : #private }
GTSpotterStepTest >> basicSearch: aString [
	
	" assert conditions before search "
	self flag: '#currentStep should be made more stable. This will fail for an empty, uninitialized spotter / but after multiple diveIn/diveOut it will be ok - inconsistent'.
	self assert: spotter currentStep == spotter currentStep.
	
	" backup preconditions "
	previousStep := spotter currentStep.
	previousContext := previousStep context.

	self shouldnt: [ spotter setText: aString from: nil ] raise: Error.
	
	" backup postconditions "
	currentStep := spotter currentStep.
	currentContext := spotter currentStep context.
	
	" assert conditions after search "
	self assert: previousStep == currentStep.
]

{ #category : #'private-navigation' }
GTSpotterStepTest >> defaultPackages [
	^ { 'GT-Spotter'. 'GT-SpotterExtensions-Core' }
]

{ #category : #convenience }
GTSpotterStepTest >> denySearch: aString [
	" if the new input is the same as before we do not expect a new search to be triggered . Therefore the context should not change "
	
	"self assert: (spotter currentStep context matches: aStringOrText)."
	
	self basicSearch: aString.
	
	" assert conditions after search "
	self assert: previousContext == currentContext.
	self assert: previousContext == spotter currentStep context.
	self assert: (previousContext matches: aString).
]

{ #category : #'private-navigation' }
GTSpotterStepTest >> implementors: aSelector [
	^ self implementors: aSelector inPackages: self defaultPackages
]

{ #category : #'private-navigation' }
GTSpotterStepTest >> implementors: aSelector inPackages: anArray [
	| implementors |
	implementors := OrderedCollection new.
	RPackageOrganizer default packages do: [ :package |
		anArray do: [ :packageName |
			package packageName = packageName ifTrue: [ 
				package methods do: [ :cm | 
					cm selector = implementors ifTrue: [ implementors add: cm ] ] ] ] ].
	^ implementors
]

{ #category : #'private-navigation' }
GTSpotterStepTest >> pragma: aSelector of: aCompiledMethod [
	^ aCompiledMethod pragmas 
		detect: [ :each | each keyword = aSelector ] 
		ifNone: [ nil ]
]

{ #category : #'private-navigation' }
GTSpotterStepTest >> pragmas: aSelector [
	^ self pragmas: aSelector inPackages: self defaultPackages
]

{ #category : #'private-navigation' }
GTSpotterStepTest >> pragmas: aSelector inPackages: anArray [
	| pragmas |
	pragmas := OrderedCollection new.
	RPackageOrganizer default packages do: [ :package |
		anArray do: [ :packageName |
			package packageName = packageName ifTrue: [ 
				package methods do: [ :cm | 
					cm methodClass isTrait ifFalse: [ 
						(cm pragmas 
							detect: [ :p | p keyword = aSelector ] 
							ifNone: [ nil ]) ifNotNil: [ :p | pragmas add: p ] ] ] ] ] ].
	^ pragmas
]

{ #category : #'private-navigation' }
GTSpotterStepTest >> senders: aSelector [
	^ self senders: aSelector inPackages: self defaultPackages
]

{ #category : #'private-navigation' }
GTSpotterStepTest >> senders: aSelector inPackages: anArray [
	| senders |
	senders := OrderedCollection new.
	RPackageOrganizer default packages do: [ :package |
		anArray do: [ :packageName |
			package packageName = packageName ifTrue: [ 
				package methods do: [ :cm | 
					(cm refersToLiteral: aSelector) ifTrue: [ 
						senders add: cm ] ] ] ] ].
	^ senders
]

{ #category : #running }
GTSpotterStepTest >> setUp [
	super setUp.
	
	spotter := GTMockSpotter new.
	
	" having an assertion within the setup is ugly, but this code has to be performed before each test begins - so we do it here instead of copy/paste-ing this line to all tests - at least it never gets forgotten when new tests are defined "
	self shouldnt: [ spotter setText: '' from: self ] raise: Error. 
	
	currentStep := currentProcessor := currentCandidate := nil
]

{ #category : #'private-navigation' }
GTSpotterStepTest >> subclassesOf: aClass filterBySubstring: aString inPackage: aPackageName [
	^ (self subclassesOf: aClass inPackage: aPackageName) select: [ :each |
		each name includesSubstring: aString caseSensitive: false ]
]

{ #category : #'private-navigation' }
GTSpotterStepTest >> subclassesOf: aClass inPackage: aPackageName [
	^ (RPackageOrganizer default packages 
		detect: [ :each | each packageName = aPackageName ] 
		ifNone: [ ^ #() ]) classes intersection: aClass withAllSubclasses
]

{ #category : #running }
GTSpotterStepTest >> tearDown [
	
	spotter := currentStep := currentProcessor := currentCandidate := nil.
	super tearDown
]

{ #category : #tests }
GTSpotterStepTest >> testContextChange [
	
	self assertSearch: 'a'.
	self assert: previousContext text = ''.
	self denySearch: 'a'.
	
	self assertSearch: 'ab'.
	self assert: previousContext text = 'a'.
	self denySearch: 'ab'.
	
	self assertSearch: 'abc'.
	self assert: previousContext text = 'ab'.
	self denySearch: 'abc'.
	
	self assertSearch: 'ab'.
	self assert: previousContext text = 'abc'.
	self denySearch: 'ab'.
	
	self assertSearch: 'a'.
	self assert: previousContext text = 'ab'.
	self denySearch: 'a'.
	
	self assertSearch: ''.
	self assert: previousContext text = 'a'.
	
	self flag: 'because the decision for triggering a search is made in multiple places, an empty input will always trigger a search - this should be simplified - one place to make the decision GTSpotter>>#setText:from: <-> #Context>>#matches:'.
	self assertSearch: ''.
	self assertSearch: ''.
	
]

{ #category : #tests }
GTSpotterStepTest >> testMock [
	" test the mock setup - the mock objects do exactly the same as the original implementation, but we replace the asynchronous implementation by a synchronous "
	self assert: spotter class = GTMockSpotter.
	self assert: spotter newStep class = GTMockSpotterStep.
	self assert: spotter newStep asyncTask class = GTMockSpotterTask.
]

{ #category : #tests }
GTSpotterStepTest >> testProcessorForClasses [
	self assertSearch: 'Collection'.
	self assertProcessorByTitle: 'Classes'.
	self assertProcessorNotEmpty.
	self assertProcessorIncludesAll: (self subclassesOf: Collection 
														filterBySubstring: 'Collection' 
														inPackage: 'Collections-Abstract')
]

{ #category : #tests }
GTSpotterStepTest >> testProcessorForImplementors [
	| partialImplementors |
	self assertSearch: 'do:'.
	self assertProcessorByTitle: 'Implementors'.
	self assertProcessorNotEmpty.
	
	partialImplementors := (SystemNavigation default allImplementorsOf: #do:) collect: [ :rm | rm compiledMethod ].
	self assertProcessorIncludesAll: partialImplementors.
	self assert: currentProcessor allFilteredCandidates size > partialImplementors size.
	
	self assertSearch: 'size'.
	self assertProcessorByTitle: 'Implementors'.
	self assertProcessorNotEmpty.
	
	partialImplementors := (SystemNavigation default allImplementorsOf: #size) collect: [ :rm | rm compiledMethod ].
	self assertProcessorIncludesAll: partialImplementors.
	self assert: currentProcessor allFilteredCandidates size > partialImplementors size.
]

{ #category : #tests }
GTSpotterStepTest >> testProcessorForPragmas [
	self assertSearch: 'spotterOrder:'.
	self assertProcessorByTitle: 'Pragmas'.
	self assertProcessorNotEmpty.
	self assert: currentProcessor allFilteredCandidates size >= 1.
	self assertCandidateMatching: [ :each | each class = PragmaType and: [ each keyword = #spotterOrder: ] ]  
]

{ #category : #tests }
GTSpotterStepTest >> testProcessorForPragmasThenDiveIn [
	self testProcessorForPragmas.
	
	self assertDiveIn.
	
	self assertSearch: ''.

	self assertProcessorByTitle: 'Pragmas'.
	self assertProcessorNotEmpty.
	self assertProcessorIncludesAll: (self pragmas: #spotterOrder:).
		
	self assertProcessorByTitle: 'Users'.
	self assertProcessorNotEmpty.
	self assertProcessorIncludesAll: (self users: #spotterOrder:).
	
	self assertSearch: 'spotterForClassesFor:'.
	
	self assertProcessorByTitle: 'Pragmas'.
	self assertProcessorNotEmpty.
	self assertProcessorIncludes: (self pragma: #spotterOrder: of: (GTSpotter >> #spotterForClassesFor:)).
	
	self assertSearch: 'extensionsPragma'.
	
	self assertProcessorByTitle: 'Users'.
	self assertProcessorNotEmpty.
	self assertProcessorIncludes: (GTSpotter class >> #extensionsPragma).
	
	self assertSearch: 'spotterForF'.
	
	self assertProcessorByTitle: 'Pragmas'.
	self assertProcessorNotEmpty.
	self assertProcessorIncludes: (self pragma: #spotterOrder: of: (GTSpotter >> #spotterForFilesFor:)).
	self assertProcessorIncludes: (self pragma: #spotterOrder: of: (FileReference lookupSelector: #spotterForFilesFor:)).
	
	self assertDiveOut.
	
	self assert: currentStep context text = 'spotterOrder:'
]

{ #category : #tests }
GTSpotterStepTest >> testProcessorForPragmasThenDiveInCategory [
	self testProcessorForPragmas.
	
	" diving into the category (without search) just reveals what was shown before - so this can be repeated infinitely and should return the same result each time "
	4 timesRepeat: [
		self assertDiveInCategory.
		self assertProcessorByTitle: 'Pragmas'.
		self assertProcessorNotEmpty.
		self assert: currentProcessor allFilteredCandidates size >= 1.
		self assertCandidateMatching: [ :each | each class = PragmaType and: [ each keyword = #spotterOrder: ] ]  ].
	
	4 timesRepeat: [
		self assert: currentStep context text = ''.
		self assertDiveOut ].
	
	self assert: currentStep context text = 'spotterOrder:'
]

{ #category : #'private-navigation' }
GTSpotterStepTest >> users: aSelector [
	^ self users: aSelector inPackages: self defaultPackages
]

{ #category : #'private-navigation' }
GTSpotterStepTest >> users: aSelector inPackages: anArray [
	| users |
	users := OrderedCollection new.
	RPackageOrganizer default packages do: [ :package |
		anArray do: [ :packageName |
			package packageName = packageName ifTrue: [ 
				package methods do: [ :cm | 
					((cm refersToLiteral: aSelector) 
						and: [ (cm hasPragmaNamed: aSelector) not ]) ifTrue: [ 
							users add: cm ] ] ] ] ].
	^ users
]
