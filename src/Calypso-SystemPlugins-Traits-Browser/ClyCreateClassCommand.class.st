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

{ #category : 'activation' }
ClyCreateClassCommand class >> fullBrowserMenuActivation [
	<classAnnotation>

	^CmdContextMenuActivation byRootGroupItemOrder: 4 for: ClyFullBrowserClassContext
]

{ #category : 'accessing' }
ClyCreateClassCommand >> defaultMenuIconName [
	^#class
]

{ #category : 'accessing' }
ClyCreateClassCommand >> defaultMenuItemName [
	^'New class'
]

{ #category : 'command execution' }
ClyCreateClassCommand >> execute [
	| classDefinition p resultTrait |
	p := package name.
	packageTag ifNotNil: [ p := p , '-' , packageTag ].
	classDefinition := ClassDefinitionPrinter fluid classDefinitionTemplateInPackage: p.
	classDefinition := self morphicUIManager
		                   multiLineRequest: 'Define class definition:'
		                   initialAnswer: classDefinition
		                   answerHeight: 250.
	classDefinition isEmptyOrNil ifTrue: [ ^ self ].
	resultTrait := browser
		               compileANewClassFrom: classDefinition
		               notifying: nil
		               startingFrom: nil.
	resultTrait ifNotNil: [ browser selectClass: resultTrait ]
]

{ #category : 'execution' }
ClyCreateClassCommand >> prepareFullExecutionInContext: aBrowserContext [
	super prepareFullExecutionInContext: aBrowserContext.

	package := aBrowserContext lastSelectedPackage.
	aBrowserContext isPackageTagSelected ifTrue: [
		packageTag := aBrowserContext lastSelectedPackageTag]
]
