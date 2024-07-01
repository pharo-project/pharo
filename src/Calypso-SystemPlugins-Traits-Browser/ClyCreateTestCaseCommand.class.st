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

{ #category : 'activation' }
ClyCreateTestCaseCommand class >> fullBrowserMenuActivation [
	<classAnnotation>

	^CmdContextMenuActivation byRootGroupItemOrder: 5 for: ClyFullBrowserClassContext
]

{ #category : 'accessing' }
ClyCreateTestCaseCommand >> defaultMenuIconName [
	^#testGreen
]

{ #category : 'accessing' }
ClyCreateTestCaseCommand >> defaultMenuItemName [
	^'New test class'
]

{ #category : 'command execution' }
ClyCreateTestCaseCommand >> execute [

	| classDefinition resultClass |
	classDefinition := ClassDefinitionPrinter fluid testClassDefinitionTemplateInPackage: package name tag: packageTag.
	classDefinition := self morphicUIManager multiLineRequest: 'Define test class:' initialAnswer: classDefinition answerHeight: 250.
	classDefinition isEmptyOrNil ifTrue: [ ^ self ].
	resultClass := browser compileANewClassFrom: classDefinition notifying: nil startingFrom: nil.
	resultClass ifNotNil: [ browser selectClass: resultClass ]
]

{ #category : 'execution' }
ClyCreateTestCaseCommand >> prepareFullExecutionInContext: aBrowserContext [
	super prepareFullExecutionInContext: aBrowserContext.

	package := aBrowserContext lastSelectedPackage.
	aBrowserContext isPackageTagSelected ifTrue: [
		packageTag := aBrowserContext lastSelectedPackageTag]
]
