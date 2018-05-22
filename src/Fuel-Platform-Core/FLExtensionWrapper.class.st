Class {
	#name : #FLExtensionWrapper,
	#superclass : #Object,
	#instVars : [
		'targetClass',
		'method',
		'selector',
		'isSource'
	],
	#category : #'Fuel-Platform-Core'
}

{ #category : #'instance creation' }
FLExtensionWrapper class >> fromPragma: aPragma [
	^ self new
		initializeWithPragma: aPragma;
		yourself
]

{ #category : #private }
FLExtensionWrapper >> compileInTarget [
	| source |
	source := self method methodClass instanceSide
		perform: self method selector
		withArguments: (1 to: self method selector numArgs) asArray.
	self targetClass organization
		classify: self selector
		under: self extensionProtocolName
]

{ #category : #private }
FLExtensionWrapper >> copySelector: aSelector ofMethod: aCompiledMethod toClass: aClass [
	aClass
		addAndClassifySelector: aSelector
		withMethod: aCompiledMethod copy
		inProtocol: self extensionProtocolName
		notifying: nil
]

{ #category : #private }
FLExtensionWrapper >> copyToTarget [
	self targetClass
		addAndClassifySelector: self selector
		withMethod: self method copy
		inProtocol: self extensionProtocolName
		notifying: nil
]

{ #category : #private }
FLExtensionWrapper >> extensionProtocolName [
	^ FLPlatform extensionProtocolName
]

{ #category : #'initialize-release' }
FLExtensionWrapper >> initializeWithPragma: aPragma [
	| argumentNamesAndValues |
	argumentNamesAndValues := self readArgumentsFrom: aPragma.
	self
		method: aPragma method copy;
		selector: (self
			readSelectorFrom: aPragma
			argumentNames: argumentNamesAndValues first
			argumentValues: argumentNamesAndValues second);
		isSource: (self
			readIsSourceFrom: aPragma
			argumentNames: argumentNamesAndValues first
			argumentValues: argumentNamesAndValues second);
		targetClass: (self
			readTargetClassFrom: aPragma
			argumentNames: argumentNamesAndValues first
			argumentValues: argumentNamesAndValues second)
]

{ #category : #operations }
FLExtensionWrapper >> install [
	self targetClass ifNil: [ ^ self ].
	
	self isSource ifTrue: [ ^ self compileInTarget ].
	self selector = self method selector ifFalse: [ ^ self renameAndCompileInTarget ].
	
	self copyToTarget
]

{ #category : #accessing }
FLExtensionWrapper >> isSource [
	^ isSource
]

{ #category : #accessing }
FLExtensionWrapper >> isSource: aBoolean [
	isSource := aBoolean
]

{ #category : #accessing }
FLExtensionWrapper >> method [
	^ method
]

{ #category : #accessing }
FLExtensionWrapper >> method: aCompiledMethod [
	method := aCompiledMethod
]

{ #category : #'initialize-release' }
FLExtensionWrapper >> readArgumentsFrom: aPragma [
	^ {
		aPragma key keywords.
		aPragma arguments }
]

{ #category : #'initialize-release' }
FLExtensionWrapper >> readIsSourceFrom: aPragma argumentNames: argumentNames argumentValues: argumentValues [
	| index |
	index := argumentNames
		indexOf: 'isSource:'
		ifAbsent: [ ^ false ].
	
	^ argumentValues at: index
]

{ #category : #'initialize-release' }
FLExtensionWrapper >> readSelectorFrom: aPragma argumentNames: argumentNames argumentValues: argumentValues [
	| index |
	index := argumentNames
		indexOf: 'selector:'
		ifAbsent: [ ^ aPragma selector ].
	
	^ argumentValues at: index
]

{ #category : #'initialize-release' }
FLExtensionWrapper >> readTargetClassFrom: aPragma argumentNames: argumentNames argumentValues: argumentValues [
	| class |
	class := Smalltalk
		at: (argumentValues at: (argumentNames indexOf: 'flExtensionOf:'))
		ifAbsent: [ ^ nil ].
		
	^ (aPragma method pragmas anySatisfy: [ :pragma |
		pragma key = #flExtensionClassSide ])
			ifTrue: [ class class ]
			ifFalse: [ class ]
]

{ #category : #private }
FLExtensionWrapper >> renameAndCompileInTarget [
	| newMethod |
	newMethod := FLPlatform current
		renamedMethodCopyOf: self method
		selector: self selector.
	self targetClass
		addSelector: self selector
		withMethod: newMethod.
	self targetClass organization
		classify: self selector
		under: self extensionProtocolName
]

{ #category : #'initialize-release' }
FLExtensionWrapper >> selector [
	^ selector
]

{ #category : #accessing }
FLExtensionWrapper >> selector: aSymbol [
	selector := aSymbol
]

{ #category : #accessing }
FLExtensionWrapper >> targetClass [
	^ targetClass
]

{ #category : #accessing }
FLExtensionWrapper >> targetClass: aClass [
	targetClass := aClass
]
