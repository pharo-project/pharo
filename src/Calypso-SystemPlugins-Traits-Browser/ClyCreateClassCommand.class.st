"
Provide a class definition to create a new class in the selected package.
"
Class {
	#name : 'ClyCreateClassCommand',
	#superclass : 'ClyBrowserCommand',
	#instVars : [
		'package',
		'packageTag'
	],
	#category : 'Calypso-SystemPlugins-Traits-Browser',
	#package : 'Calypso-SystemPlugins-Traits-Browser'
}

{ #category : 'testing' }
ClyCreateClassCommand class >> canBeExecutedInContext: aBrowserContext [
	(super canBeExecutedInContext: aBrowserContext) ifFalse: [ ^false ].

	^aBrowserContext isPackageSelected
]

{ #category : 'execution' }
ClyCreateClassCommand >> executeRefactoring [

	| classDefinition resultTrait |
	classDefinition := ClassDefinitionPrinter fluid classDefinitionTemplateInPackage: package name tag: packageTag named: #MyClass.
	classDefinition := self morphicUIManager multiLineRequest: 'Define class definition:' initialAnswer: classDefinition answerHeight: 250.
	classDefinition isEmptyOrNil ifTrue: [ ^ self ].
	resultTrait := self browser compileANewClassFrom: classDefinition notifying: nil startingFrom: nil.
	resultTrait ifNotNil: [ self browser selectClass: resultTrait ]
]

{ #category : 'accessing' }
ClyCreateClassCommand >> iconName [
	^#class
]

{ #category : 'testing' }
ClyCreateClassCommand >> isAvailableInMenuOf: aToolContext [

	^ aToolContext isFullBrowserClassContext
]

{ #category : 'accessing' }
ClyCreateClassCommand >> name [
	^'New class'
]

{ #category : 'accessing' }
ClyCreateClassCommand >> order [

	^ 4
]

{ #category : 'execution' }
ClyCreateClassCommand >> prepareFullExecution [

	super prepareFullExecution.
	package := self context lastSelectedPackage.
	self context isPackageTagSelected ifTrue: [ packageTag := self context lastSelectedPackageTag ]
]
