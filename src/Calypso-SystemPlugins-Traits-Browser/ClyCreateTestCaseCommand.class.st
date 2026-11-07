"
Generate New Test Class: Automatically create a test class for the selected class, to include test methods and setup/teardown stubs.
"
Class {
	#name : 'ClyCreateTestCaseCommand',
	#superclass : 'ClyBrowserCommand',
	#instVars : [
		'package',
		'packageTag'
	],
	#category : 'Calypso-SystemPlugins-Traits-Browser',
	#package : 'Calypso-SystemPlugins-Traits-Browser'
}

{ #category : 'testing' }
ClyCreateTestCaseCommand class >> canBeExecutedInContext: aBrowserContext [
	(super canBeExecutedInContext: aBrowserContext) ifFalse: [ ^false ].

	^aBrowserContext isPackageSelected
]

{ #category : 'execution' }
ClyCreateTestCaseCommand >> executeRefactoring [

	| classDefinition resultClass |
	classDefinition := ClassDefinitionPrinter fluid testClassDefinitionTemplateInPackage: package name tag: packageTag.
	classDefinition := self morphicUIManager multiLineRequest: 'Define test class:' initialAnswer: classDefinition answerHeight: 250.
	classDefinition isEmptyOrNil ifTrue: [ ^ self ].
	resultClass := self browser compileANewClassFrom: classDefinition notifying: nil startingFrom: nil.
	resultClass ifNotNil: [ self browser selectClass: resultClass ]
]

{ #category : 'accessing' }
ClyCreateTestCaseCommand >> iconName [
	^#testGreen
]

{ #category : 'testing' }
ClyCreateTestCaseCommand >> isAvailableInMenuOf: aToolContext [

	^ aToolContext isFullBrowserClassContext
]

{ #category : 'accessing' }
ClyCreateTestCaseCommand >> name [
	^'New test class'
]

{ #category : 'accessing' }
ClyCreateTestCaseCommand >> order [

	^ 5
]

{ #category : 'execution' }
ClyCreateTestCaseCommand >> prepareFullExecution [

	super prepareFullExecution.
	package := self context lastSelectedPackage.
	self context isPackageTagSelected ifTrue: [ packageTag := self context lastSelectedPackageTag ]
]
