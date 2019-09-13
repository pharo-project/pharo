"
SUnit tests for Gofer operations
"
Class {
	#name : #GoferOperationTest,
	#superclass : #GoferTest,
	#instVars : [
		'environment'
	],
	#category : #'Gofer-Tests-Tests'
}

{ #category : #utilities }
GoferOperationTest >> allManagers [

	^ MCWorkingCopy allManagers 
	
]

{ #category : #accessing }
GoferOperationTest >> environment [
	^ environment
]

{ #category : #accessing }
GoferOperationTest >> environment: anObject [
	environment := anObject
]

{ #category : #utilities }
GoferOperationTest >> hasClass: aSymbol [
	^ environment includesKey: aSymbol
]

{ #category : #utilities }
GoferOperationTest >> hasClass: aSymbol selector: aSelector [
	^ (environment classNamed: aSymbol) includesSelector: aSelector
]

{ #category : #utilities }
GoferOperationTest >> hasPackage: aString [
	^ self allManagers anySatisfy: [ :package | package packageName = aString ]
]

{ #category : #utilities }
GoferOperationTest >> hasVersion: aString [
	^ self allManagers anySatisfy: [ :version | version ancestry ancestorString = aString ]
]

{ #category : #running }
GoferOperationTest >> setUp [
	super setUp.
	self setUpEnvironment.
	gofer repository: self monticelloRepository
]

{ #category : #running }
GoferOperationTest >> setUpEnvironment [
	environment := self class environment.
]

{ #category : #running }
GoferOperationTest >> tearDown [
	(self hasPackage: 'GoferFoo')
		ifTrue: [ Gofer new package: 'GoferFoo'; unload ].
	(self hasPackage: 'GoferBar')
		ifTrue: [ Gofer new package: 'GoferBar'; unload ].
	(self hasPackage: 'ConfigurationOfGoferFoo')
		ifTrue: [ Gofer new package: 'ConfigurationOfGoferFoo'; unload ].
	super tearDown.
]

{ #category : #tests }
GoferOperationTest >> testCleanup [
	| class |
	gofer
		package: 'GoferFoo';
		load.
	class := environment classNamed: #GoferFoo.
	environment organization addCategory: #'GoferFoo-Empty'.
	class organization addCategory: #empty.
	class class organization addCategory: #empty.
	gofer cleanup.
	self deny: (Smalltalk organization categories includes: #'GoferFoo-Empty').
	self deny: (class organization categories includes: #'GoferFoo-Empty').
	self deny: (class class organization categories includes: #'GoferFoo-Empty')
]

{ #category : #tests }
GoferOperationTest >> testCommit [
	| repository |
	repository := MCDictionaryRepository new.
	gofer
		package: 'GoferFoo';
		load.
	gofer := Gofer new.
	gofer 
		disablePackageCache;
		repository: repository;
		package: 'GoferFoo';
		commit: 'A test commit'.
	self assert: repository allVersionInfos size equals: 1
]

{ #category : #tests }
GoferOperationTest >> testFetch [
	gofer package: 'GoferFoo'.
	gofer fetch
]

{ #category : #tests }
GoferOperationTest >> testLoad [
	gofer version: 'GoferFoo-lr.1'.
	gofer load.
	self assert: (self hasVersion: 'GoferFoo-lr.1').
	self assert: (self hasClass: #GoferFoo)
]

{ #category : #tests }
GoferOperationTest >> testLocalChanges [
	| changes |
	gofer
		package: 'GoferBar';
		load.
	(environment classNamed: #GoferBar) compile: 'foo'.
	changes := gofer localChanges.
	self assert: changes operations size equals: 1
]

{ #category : #tests }
GoferOperationTest >> testMerge [
	| initial |
	initial := gofer copy.
	initial
		version: 'GoferBar-jf.1';
		load.
	gofer
		package: 'GoferBar';
		load.
	(environment classNamed: #GoferBar) compile: 'foo'.
	[ gofer merge ]
		on: ProvideAnswerNotification
		do: [ :e | e resume: true ].
	self assert: (self hasClass: #GoferBar selector: #foo)
]

{ #category : #tests }
GoferOperationTest >> testPush [
	| repository |
	gofer := Gofer new.
	gofer disablePackageCache.
	gofer repository: (repository := MCDictionaryRepository new).
	gofer package: 'GoferFoo'.
	gofer push
]

{ #category : #tests }
GoferOperationTest >> testRecompile [
	gofer package: 'Gofer-Core'.
	gofer recompile
]

{ #category : #tests }
GoferOperationTest >> testReinitialize [
	| class |
	gofer
		package: 'GoferFoo';
		load.
	class := Smalltalk classNamed: #GoferFoo.
	class
		addClassVarNamed: #InstanceSide;
		addClassVarNamed: #ClassSide.
	class compile: 'initialize InstanceSide := true'.
	class class compile: 'initialize ClassSide := true'.
	self assert: (class classPool at: #InstanceSide) isNil.
	self assert: (class classPool at: #ClassSide) isNil.
	gofer reinitialize.
	self assert: (class classPool at: #InstanceSide) isNil.
	self assert: (class classPool at: #ClassSide)
]

{ #category : #tests }
GoferOperationTest >> testRemoteChanges [
	| changes |
	gofer
		package: 'GoferBar';
		load.
	(environment classNamed: #GoferBar) compile: 'foo'.
	changes := gofer remoteChanges.
	self assert: changes operations size equals: 1
]

{ #category : #tests }
GoferOperationTest >> testRevert [
	gofer
		package: 'GoferFoo';
		package: 'GoferBar';
		load.
	(environment classNamed: #GoferBar) category: 'GoferFoo'.
	gofer revert.
	self assert: (environment classNamed: #GoferFoo) category asSymbol equals: #GoferFoo.
	self assert: (environment classNamed: #GoferBar) category asSymbol equals: #GoferBar
]

{ #category : #tests }
GoferOperationTest >> testUnload [
	gofer
		package: 'GoferFoo';
		load.
	gofer unload.
	self deny: (self hasPackage: 'GoferFoo').
	self deny: (self hasClass: #GoferFoo)
]

{ #category : #tests }
GoferOperationTest >> testUpdate [
	| initial |
	initial := gofer copy.
	initial
		version: 'GoferFoo-lr.1';
		load.
	gofer package: 'GoferFoo'.
	gofer update.
	self assert: (self hasVersion: 'GoferFoo-lr.4')
]
