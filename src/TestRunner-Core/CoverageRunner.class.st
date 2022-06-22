"
I am a test Coverage runner

# Public API
- addClass:
- addTestCase:
- run
- coverage
"
Class {
	#name : #CoverageRunner,
	#superclass : #Object,
	#instVars : [
		'classes',
		'link',
		'methods',
		'covered',
		'uncovered',
		'suites',
		'runner'
	],
	#category : #'TestRunner-Core-Core'
}

{ #category : #adding }
CoverageRunner >> addClass: aClass [

	self classes add: aClass.
	self updateMethods: aClass
]

{ #category : #coverage }
CoverageRunner >> addLinkToAllMethodsFromClasses [

	methods do: [ :method | method ast link: link ]
]

{ #category : #adding }
CoverageRunner >> addTestCase: aTestCase [

	self testSuitesFromClass: aTestCase
]

{ #category : #coverage }
CoverageRunner >> allClassesMethods [

	| res |
	res := OrderedCollection new.
	self classes do: [ :class | res addAll: class methods ].
	^ res
]

{ #category : #coverage }
CoverageRunner >> classes [

	^ classes ifNil: [ classes := OrderedCollection new ]
]

{ #category : #coverage }
CoverageRunner >> coverage [

	self classes isEmpty ifTrue: [ self error: 'Any classes to run' ].
	^ 100 - (100 * self unCovered size // self methods size)
]

{ #category : #adding }
CoverageRunner >> defaultRunner [

	^ TestRunner2 new
]

{ #category : #testing }
CoverageRunner >> hasClassToRun [

	^ self classes isNotEmpty
]

{ #category : #initialization }
CoverageRunner >> initialize [

	super initialize.
	link := MetaLink new
		        selector: #tagExecuted;
		        metaObject: #node
]

{ #category : #coverage }
CoverageRunner >> initializeMethods [
	methods := self allClassesMethods.
]

{ #category : #accessing }
CoverageRunner >> link [
	^ link
]

{ #category : #coverage }
CoverageRunner >> methods [

	^ methods ifNil: [ methods := OrderedCollection new ]
]

{ #category : #coverage }
CoverageRunner >> removeAnnotationsFromMethods [

	self methods do: [ :each | 
		each ast removeProperty: #tagExecuted ifAbsent: [  ] ]
]

{ #category : #running }
CoverageRunner >> run [

	[ 
	self setUp.
	self runTestSuites ] ensure: [ self tearDown ]
]

{ #category : #running }
CoverageRunner >> runSuite: aTestSuite [

	self runner runSuite: aTestSuite
]

{ #category : #running }
CoverageRunner >> runTestSuites [

	self suites do: [ :aSuite | self runSuite: aSuite ]
]

{ #category : #getter }
CoverageRunner >> runner [

	^ runner ifNil: [ self defaultRunner ]
]

{ #category : #coverage }
CoverageRunner >> saveCoveredMethods [

	covered := methods select: [ :each | each ast hasBeenExecuted ]
]

{ #category : #coverage }
CoverageRunner >> saveUnCoveredMethods [

	uncovered := methods reject: [ :each | each ast hasBeenExecuted ]
]

{ #category : #coverage }
CoverageRunner >> setUp [
	
	self addLinkToAllMethodsFromClasses.
]

{ #category : #getter }
CoverageRunner >> suites [

	^ suites ifNil: [ suites := OrderedCollection new ]
]

{ #category : #running }
CoverageRunner >> tearDown [

	link uninstall.
	self saveCoveredMethods.
	self saveUnCoveredMethods.
	self removeAnnotationsFromMethods
]

{ #category : #tests }
CoverageRunner >> testSuitesFromClass: aClass [

	self suites add: aClass suite
]

{ #category : #coverage }
CoverageRunner >> unCovered [

	^ uncovered ifNil: [ uncovered := OrderedCollection new ]
]

{ #category : #adding }
CoverageRunner >> updateMethods: aClass [

	self methods addAll: aClass methods
]
