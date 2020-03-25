Class {
	#name : #ClyCreateClassCommand,
	#superclass : #ClyBrowserCommand,
	#instVars : [
		'package',
		'classTag'
	],
	#category : #'Calypso-SystemPlugins-Traits-Browser'
}

{ #category : #testing }
ClyCreateClassCommand class >> canBeExecutedInContext: aBrowserContext [
	(super canBeExecutedInContext: aBrowserContext) ifFalse: [ ^false ].
	
	^aBrowserContext isPackageSelected 
]

{ #category : #activation }
ClyCreateClassCommand class >> fullBrowserMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemOrder: 4 for: ClyClassContextOfFullBrowser 
]

{ #category : #accessing }
ClyCreateClassCommand >> defaultMenuIconName [
	^#class
]

{ #category : #accessing }
ClyCreateClassCommand >> defaultMenuItemName [
	^'New class'
]

{ #category : #execution }
ClyCreateClassCommand >> execute [
	| classDefinition p resultTrait |
	p := package name.
	classTag ifNotNil: [ p := p , '-' , classTag ].
	classDefinition := 'Object subclass: #MyClass
	instanceVariableNames: ''''
	classVariableNames: ''''
	package: ''' , p , ''''.
	classDefinition := UIManager default
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

{ #category : #execution }
ClyCreateClassCommand >> prepareFullExecutionInContext: aBrowserContext [
	super prepareFullExecutionInContext: aBrowserContext.
	
	package := aBrowserContext lastSelectedPackage.
	aBrowserContext isClassTagSelected ifTrue: [ 
		classTag := aBrowserContext lastSelectedClassTag]
]
