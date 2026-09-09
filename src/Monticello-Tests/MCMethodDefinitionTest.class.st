Class {
	#name : 'MCMethodDefinitionTest',
	#superclass : 'MCTestCase',
	#instVars : [
		'loader'
	],
	#category : 'Monticello-Tests-Base',
	#package : 'Monticello-Tests',
	#tag : 'Base'
}

{ #category : 'running' }
MCMethodDefinitionTest >> extensionPackageName [

	^ 'MonticelloTestsFirstExtensionMockPackage'
]

{ #category : 'accessing' }
MCMethodDefinitionTest >> mockClass [

	^ self class classInstaller make: [ :builder |
		  builder
			  name: 'MonticelloTestsMockClass';
			  package: self mockPackageName ]
]

{ #category : 'accessing' }
MCMethodDefinitionTest >> mockPackageName [

	^ 'MonticelloTestsMockPackage'
]

{ #category : 'running' }
MCMethodDefinitionTest >> secondExtensionPackageName [

	^ 'MonticelloTestsSecondExtensionMockPackage-override'
]

{ #category : 'running' }
MCMethodDefinitionTest >> setUp [

	super setUp.
	self testingEnvironment: SystemEnvironment new.
	loader := MCPackageLoader new.
	loader targetEnvironment: testingEnvironment.
]

{ #category : 'running' }
MCMethodDefinitionTest >> tearDown [

	{ self secondExtensionPackageName . self extensionPackageName . self mockPackageName } do: [ :packageName | self packageOrganizer removePackage: packageName ].
	super tearDown
]

{ #category : 'testing' }
MCMethodDefinitionTest >> testCannotLoad [

	| definition |
	definition := self
		              mockMethod: #kjahs87
		              class: 'NoSuchClass'
		              source: 'kjahs87 ^self'
		              meta: false.
	[
	definition loadWithLoader: loader.
	self fail ]
		on: Error
		do: [ :error |
			self
				assert: error messageText
				equals: 'Class not found: #NoSuchClass' ].
	self assertEmpty: (SystemNavigation new allImplementorsOf: #kjahs87)
]

{ #category : 'testing' }
MCMethodDefinitionTest >> testCannotLoadInvalidCodeInStrictMode [

	| definition mockClass |
	testingEnvironment
		at: #NoSuchClass
		put: (mockClass := self mockClass).

	"The source does not parse"
	definition := self
		              mockMethod: #kjahs87
		              class: 'NoSuchClass'
		              source: 'kjahs87 a :='
		              meta: false.

	loader beStrict.
	[
	definition loadWithLoader: loader.
	self fail ]
		on: OCCompilationError
		do: [ :error |
			self
				assert: error compilationResult ast allNotices first messageText
				equals: 'Variable or expression expected' ]
]

{ #category : 'testing' }
MCMethodDefinitionTest >> testCannotLoadUndeclaredInStrictMode [

	| definition mockClass |
	testingEnvironment
		at: #NoSuchClass
		put: (mockClass := self mockClass).

	definition := self
		              mockMethod: #kjahs87
		              class: 'NoSuchClass'
		              source: 'kjahs87 ^UNDECLARED'
		              meta: false.

	loader beStrict.
	[
	definition loadWithLoader: loader.
	self fail ]
		on: OCCompilationError
		do: [ :error |
			self assert: error compilationResult ast allNotices first messageText equals: 'Undeclared variable named UNDECLARED in kjahs87' ]
]

{ #category : 'testing' }
MCMethodDefinitionTest >> testComparison [
	|d1 d2 d3 d4 d5 |
	d1 := self mockMethod: #one class: 'A' source: '1' meta: false.
	d2 := self mockMethod: #one class: 'A' source: '2' meta: false.
	d3 := self mockMethod: #one class: 'A' source: '1' meta: true.
	d4 := self mockMethod: #two class: 'A' source: '1' meta: false.
	d5 := self mockMethod: #two class: 'A' source: '1' meta: false.
	
	self assert: (d1 isRevisionOf: d2).
	self deny: (d1 isSameRevisionAs: d2).
	
	self deny: (d1 isRevisionOf: d3).
	self deny: (d1 isRevisionOf: d4).
	
	self assert: (d4 isSameRevisionAs: d5).
]

{ #category : 'testing' }
MCMethodDefinitionTest >> testLoadAndUnload [

	| definition mockClass mockClassInstance |
	mockClass := self mockClass.
	testingEnvironment
		at: mockClass name
		put: mockClass.
	
	mockClassInstance := mockClass new.
	mockClass compile: 'one ^ 1'.
	definition := self
		              mockMethod: #one
		              class: mockClass name
		              source: 'one ^2'
		              meta: false.
	self assert: mockClassInstance one equals: 1.
	definition loadWithLoader: loader.
	self assert: mockClassInstance one equals: 2.
	definition unload.
	self deny: (mockClassInstance respondsTo: #one)
]

{ #category : 'testing' }
MCMethodDefinitionTest >> testLoadInvalidCodeInLegacyMode [

	| definition mockClass |
	
	<ignoreNotImplementedSelectors: #( kjahs87 )>
	
	testingEnvironment
		at: #NoSuchClass
		put: (mockClass := self mockClass).

	"The source does not parse"
	definition := self
		              mockMethod: #kjahs87
		              class: 'NoSuchClass'
		              source: 'kjahs87 a :='
		              meta: false.

	definition loadWithLoader: loader.

	[
	mockClass new kjahs87.
	self fail ]
		on: OCRuntimeSyntaxError
		do: [ :error |
			self
				assert: error messageText
				equals: 'Variable or expression expected' ]
]

{ #category : 'testing' }
MCMethodDefinitionTest >> testLoadUndeclaredInLegacyMode [

	| definition mockClass |
	testingEnvironment
		at: #NoSuchClass
		put: (mockClass := self mockClass).

	definition := self
		              mockMethod: #kjahs87
		              class: 'NoSuchClass'
		              source: 'kjahs87 ^UNDECLARED'
		              meta: false.

	definition loadWithLoader: loader.

	self assert:
		(mockClass >> #kjahs87) literals first isUndeclaredVariable
]

{ #category : 'testing' }
MCMethodDefinitionTest >> testMethodDefinitionWithEmptyProtocolIsClassifiedAsAsYetUnclassified [

	| methodDef |
	methodDef := MCMethodDefinition
		             className: #Object
		             classIsMeta: false
		             selector: #name
		             category: ''
		             timeStamp: nil
		             source: 'name
	^ self printString'.
	self assert: methodDef protocol equals: Protocol unclassified
]
