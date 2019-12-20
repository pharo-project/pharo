Class {
	#name : #MetaLinkObjectAPITest,
	#superclass : #TestCase,
	#instVars : [
		'tag'
	],
	#category : #'Reflectivity-Tests'
}

{ #category : #initialization }
MetaLinkObjectAPITest >> link [
	^ MetaLink new
		metaObject: self;
		selector: #tagExec:;
		arguments: #(object);
		control: #after
]

{ #category : #initialization }
MetaLinkObjectAPITest >> linkForTemp [
	| okNode |
	okNode := RBLiteralValueNode value: 'ok'.
	^ MetaLink new
		metaObject: self;
		selector: #tagExec:;
		arguments: (Array with: okNode);
		control: #after
]

{ #category : #running }
MetaLinkObjectAPITest >> setUp [
	super setUp.
	tag := OrderedCollection new.
	MetaLink uninstallAll 
]

{ #category : #tagging }
MetaLinkObjectAPITest >> tagExec: aTag [
	tag add: aTag
]

{ #category : #running }
MetaLinkObjectAPITest >> tearDown [
	MetaLink uninstallAll.
	super tearDown.
]

{ #category : #'class api' }
MetaLinkObjectAPITest >> testLinkClassToAST [
	| link instance |
	link := self link.

	ReflectivityExamples2 link: link toAST: (ReflectivityExamples2 >> #exampleMethod) ast.
	self assert: tag isEmpty.
	instance := ReflectivityExamples2 new.
	instance exampleMethod.
	self assert: tag size equals: 1.
	self assert: tag first identicalTo: instance
]

{ #category : #'class api' }
MetaLinkObjectAPITest >> testLinkClassToClassVarNamed [
	| link instance |
	link := self link.

	ReflectivityExamples2 link: link toClassVariableNamed: #classVar.
	self assert: tag isEmpty.

	instance := ReflectivityExamples2 new.
	instance methodWithClassVarAccess.

	self assert: tag size equals: 2.
	self assert: (tag allSatisfy: [ :t | t == instance ])
]

{ #category : #'class api' }
MetaLinkObjectAPITest >> testLinkClassToSlotNamed [
	| link instance |
	link := self link.

	ReflectivityExamples2 link: link toSlotNamed: #instVar.
	self assert: tag isEmpty.

	instance := ReflectivityExamples2 new.
	instance methodWithInstVarAccess.

	self assert: tag size equals: 2.
	self assert: (tag allSatisfy: [ :t | t == instance ])
]

{ #category : #'class api' }
MetaLinkObjectAPITest >> testLinkClassToTempVarNamed [
	| link instance |
	link := self linkForTemp.

	ReflectivityExamples2 link: link toTemporaryNamed: #temp inMethod: #methodWithTempVarAccess.
	self assert: tag isEmpty.

	instance := ReflectivityExamples2 new.
	instance methodWithTempVarAccess.

	self assert: tag size equals: 4.
	self assert: (tag allSatisfy: [ :t | t = 'ok' ])
]

{ #category : #'object - api' }
MetaLinkObjectAPITest >> testLinkObjectToAST [
	| link instance instance2 |
	link := self link.

	instance := ReflectivityExamples2 new.
	instance2 := ReflectivityExamples2 new.

	instance link: link toAST: (ReflectivityExamples2 >> #exampleMethod) ast.
	self assert: tag isEmpty.

	instance exampleMethod.
	instance2 exampleMethod.
	self assert: tag size equals: 1.
	self assert: tag first identicalTo: instance
]

{ #category : #'object - api' }
MetaLinkObjectAPITest >> testLinkObjectToClassVarName [
	| link instance |
	self skip.
	self flag: 'must be fixed'.

	link := self link.

	instance := ReflectivityExamples2 new.

	instance link: link toClassVariableNamed: #classVar.
	self assert: tag isEmpty.

	instance methodWithClassVarAccess.
	ReflectivityExamples2 new methodWithClassVarAccess.

	self assert: tag size equals: 4.
	self assert: (tag allSatisfy: [ :t | t == instance ])
]

{ #category : #'object - api' }
MetaLinkObjectAPITest >> testLinkObjectToSlotNamed [
	| link instance |
	link := self link.

	instance := ReflectivityExamples2 new.

	instance link: link toSlotNamed: #instVar.
	self assert: tag isEmpty.

	instance methodWithInstVarAccess.
	ReflectivityExamples2 new methodWithInstVarAccess.

	self assert: tag size equals: 4.
	self assert: (tag allSatisfy: [ :t | t == instance ])
]

{ #category : #'object - api' }
MetaLinkObjectAPITest >> testLinkObjectToTempVarName [
	| link instance |
	link := self link.

	instance := ReflectivityExamples2 new.

	instance link: link toTemporaryNamed: #temp inMethod: #methodWithTempVarAccess.
	self assert: tag isEmpty.

	instance methodWithTempVarAccess.
	ReflectivityExamples2 new methodWithTempVarAccess.

	self assert: tag size equals: 4.
	self assert: (tag allSatisfy: [ :t | t == instance ])
]

{ #category : #'common api' }
MetaLinkObjectAPITest >> testLinkToClassVarNameAll [
	| link instance |
	link := self link.

	"For all instances"
	instance := ReflectivityExamples2 new.
	ReflectivityExamples2 link: link toClassVariableNamed: #classVar option: #all.
	self assert: tag isEmpty.

	instance methodWithClassVarAccess.

	self assert: tag size equals: 4.
	self assert: (tag allSatisfy: [ :t | t == instance ]).

	"For instance only"
	link uninstall.
	tag := OrderedCollection new.
	instance link: link toClassVariableNamed: #classVar option: #all.

	instance methodWithClassVarAccess.
	ReflectivityExamples2 new methodWithClassVarAccess.

	self assert: tag size equals: 4.
	self assert: (tag allSatisfy: [ :t | t == instance ])
]

{ #category : #'common api' }
MetaLinkObjectAPITest >> testLinkToClassVarNameReads [
	| link instance |
	link := self link.

	"For all instances"
	instance := ReflectivityExamples2 new.
	ReflectivityExamples2 link: link toClassVariableNamed: #classVar option: #read.
	self assert: tag isEmpty.

	instance methodWithClassVarAccess.

	self assert: tag size equals: 2.
	self assert: (tag allSatisfy: [ :t | t == instance ]).

	"For instance only"
	link uninstall.
	tag := OrderedCollection new.
	instance link: link toClassVariableNamed: #classVar option: #read.

	instance methodWithClassVarAccess.
	ReflectivityExamples2 new methodWithClassVarAccess.

	self assert: tag size equals: 2.
	self assert: (tag allSatisfy: [ :t | t == instance ])
]

{ #category : #'common api' }
MetaLinkObjectAPITest >> testLinkToClassVarNameWrites [
	| link instance |
	link := self link.

	"For all instances"
	instance := ReflectivityExamples2 new.
	ReflectivityExamples2 link: link toClassVariableNamed: #classVar option: #write.
	self assert: tag isEmpty.

	instance methodWithClassVarAccess.

	self assert: tag size equals: 2.
	self assert: (tag allSatisfy: [ :t | t == instance ]).

	"For instance only"
	link uninstall.
	tag := OrderedCollection new.
	instance link: link toClassVariableNamed: #classVar option: #write.

	instance methodWithClassVarAccess.
	ReflectivityExamples2 new methodWithClassVarAccess.

	self assert: tag size equals: 2.
	self assert: (tag allSatisfy: [ :t | t == instance ])
]

{ #category : #'common api' }
MetaLinkObjectAPITest >> testLinkToSlotNamedAll [
	| link instance |
	link := self link.
	instance := ReflectivityExamples2 new.

	"For all instances"
	ReflectivityExamples2 link: link toSlotNamed: #instVar option: #all.
	self assert: tag isEmpty.

	instance methodWithInstVarAccess.

	self assert: tag size equals: 4.
	self assert: (tag allSatisfy: [ :t | t == instance ]).

	"For instance only"
	link uninstall.
	tag := OrderedCollection new.
	instance link: link toSlotNamed: #instVar option: #all.

	instance methodWithInstVarAccess.
	ReflectivityExamples2 new methodWithInstVarAccess.

	self assert: tag size equals: 4.
	self assert: (tag allSatisfy: [ :t | t == instance ])
]

{ #category : #'common api' }
MetaLinkObjectAPITest >> testLinkToSlotNamedReads [
	| link instance |
	link := self link.
	instance := ReflectivityExamples2 new.

	"For all instances"
	ReflectivityExamples2 link: link toSlotNamed: #instVar option: #read.
	self assert: tag isEmpty.

	instance methodWithInstVarAccess.

	self assert: tag size equals: 2.
	self assert: (tag allSatisfy: [ :t | t == instance ]).

	"For instance only"
	link uninstall.
	tag := OrderedCollection new.
	instance link: link toSlotNamed: #instVar option: #read.

	instance methodWithInstVarAccess.
	ReflectivityExamples2 new methodWithInstVarAccess.

	self assert: tag size equals: 2.
	self assert: (tag allSatisfy: [ :t | t == instance ])
]

{ #category : #'common api' }
MetaLinkObjectAPITest >> testLinkToSlotNamedWrites [
	| link instance |
	link := self link.
	instance := ReflectivityExamples2 new.

	"For all instances"
	ReflectivityExamples2 link: link toSlotNamed: #instVar option: #write.
	self assert: tag isEmpty.

	instance methodWithInstVarAccess.

	self assert: tag size equals: 2.
	self assert: (tag allSatisfy: [ :t | t == instance ]).

	"For instance only"
	link uninstall.
	tag := OrderedCollection new.
	instance link: link toSlotNamed: #instVar option: #write.

	instance methodWithInstVarAccess.
	ReflectivityExamples2 new methodWithInstVarAccess.

	self assert: tag size equals: 2.
	self assert: (tag allSatisfy: [ :t | t == instance ])
]

{ #category : #'common api' }
MetaLinkObjectAPITest >> testLinkToTempVarNameAll [
	| link instance |
	link := self link.
	instance := ReflectivityExamples2 new.

	"For all instances"
	ReflectivityExamples2
		link: link
		toTemporaryNamed: #temp
		inMethod: #methodWithTempVarAccess
		option: #all.
	self assert: tag isEmpty.

	instance methodWithTempVarAccess.

	self assert: tag size equals: 4.
	self assert: (tag allSatisfy: [ :t | t == instance ]).

	"For instance only"
	link uninstall.
	tag := OrderedCollection new.
	instance
		link: link
		toTemporaryNamed: #temp
		inMethod: #methodWithTempVarAccess
		option: #all.

	instance methodWithTempVarAccess.
	ReflectivityExamples2 new methodWithTempVarAccess.

	self assert: tag size equals: 4.
	self assert: (tag allSatisfy: [ :t | t == instance ])
]

{ #category : #'common api' }
MetaLinkObjectAPITest >> testLinkToTempVarNameReads [
	| link instance |
	link := self link.
	instance := ReflectivityExamples2 new.

	"For all instances"
	ReflectivityExamples2
		link: link
		toTemporaryNamed: #temp
		inMethod: #methodWithTempVarAccess
		option: #read.
	self assert: tag isEmpty.

	instance methodWithTempVarAccess.

	self assert: tag size equals: 2.
	self assert: (tag allSatisfy: [ :t | t == instance ]).

	"For instance only"
	link uninstall.
	tag := OrderedCollection new.
	instance
		link: link
		toTemporaryNamed: #temp
		inMethod: #methodWithTempVarAccess
		option: #read.

	instance methodWithTempVarAccess.
	ReflectivityExamples2 new methodWithTempVarAccess.

	self assert: tag size equals: 2.
	self assert: (tag allSatisfy: [ :t | t == instance ])
]

{ #category : #'common api' }
MetaLinkObjectAPITest >> testLinkToTempVarNameWrites [
	| link instance |
	link := self link.
	instance := ReflectivityExamples2 new.

	"For all instances"
	ReflectivityExamples2
		link: link
		toTemporaryNamed: #temp
		inMethod: #methodWithTempVarAccess
		option: #write.
	self assert: tag isEmpty.

	instance methodWithTempVarAccess.

	self assert: tag size equals: 2.
	self assert: (tag allSatisfy: [ :t | t == instance ]).

	"For instance only"
	link uninstall.
	tag := OrderedCollection new.
	instance
		link: link
		toTemporaryNamed: #temp
		inMethod: #methodWithTempVarAccess
		option: #write.

	instance methodWithTempVarAccess.
	ReflectivityExamples2 new methodWithTempVarAccess.

	self assert: tag size equals: 2.
	self assert: (tag allSatisfy: [ :t | t == instance ])
]
