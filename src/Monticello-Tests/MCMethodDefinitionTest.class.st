Class {
	#name : 'MCMethodDefinitionTest',
	#superclass : 'MCTestCase',
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
	self should: [ definition load ] raise: Error.
	self assertEmpty: (SystemNavigation new allImplementorsOf: #kjahs87)
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
	mockClassInstance := mockClass new.
	mockClass compile: 'one ^ 1'.
	definition := self
		              mockMethod: #one
		              class: mockClass name
		              source: 'one ^2'
		              meta: false.
	self assert: mockClassInstance one equals: 1.
	definition load.
	self assert: mockClassInstance one equals: 2.
	definition unload.
	self deny: (mockClassInstance respondsTo: #one)
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

{ #category : 'testing' }
MCMethodDefinitionTest >> testPartiallyRevertOverrideMethod [

	| definition mockClass mockClassInstance |
	<ignoreNotImplementedSelectors: #(override)>
	mockClass := self mockClass.
	mockClassInstance := mockClass new.
	mockClass compile: 'override ^ 2' classified: '*' , self extensionPackageName.
	mockClass compile: 'override ^ 3' classified: '*' , self secondExtensionPackageName.
	mockClass compile: 'override ^ 4' classified: '*' , self secondExtensionPackageName.
	definition := (mockClass >> #override) asMCMethodDefinition.
	self assert: definition isOverrideMethod.
	self assert: mockClassInstance override equals: 4.
	definition unload.
	self assert: mockClassInstance override equals: 2.
	self assert: (mockClass >> #override) protocolName equals: '*' , self extensionPackageName
]

{ #category : 'testing' }
MCMethodDefinitionTest >> testRevertOldMethod [

	| definition changeRecord mockClass |
	mockClass := self mockClass.
	mockClass compile: 'yourself ^ self' classified: 'accessing'.
	mockClass compile: 'yourself ^ self' classified: '*MonticelloMocks'.
	definition := (mockClass >> #yourself) asMCMethodDefinition.
	changeRecord := definition overridenMethodOrNil.
	self assert: changeRecord isNotNil.
	self assert: changeRecord protocol equals: 'accessing'.
	changeRecord fileIn
]

{ #category : 'testing' }
MCMethodDefinitionTest >> testRevertOverrideMethod [

	| definition mockClass mockClassInstance |
	<ignoreNotImplementedSelectors: #(override)>
	mockClass := self mockClass.
	mockClassInstance := mockClass new.
	mockClass compile: 'override ^ 1' classified: 'mocks'.
	mockClass compile: 'override ^ 2' classified: '*' , self secondExtensionPackageName.
	definition := (mockClass >> #override) asMCMethodDefinition.
	self assert: definition isOverrideMethod.
	self assert: mockClassInstance override equals: 2.
	definition unload.
	self assert: mockClassInstance override equals: 1.
	self assert: (RGMethodDefinition realClass: mockClass selector: #override) protocol equals: 'mocks'
]
