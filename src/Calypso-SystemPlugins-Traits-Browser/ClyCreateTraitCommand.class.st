"
I am a command to create new trait in selected package.
I am a copy of command from Nautilus.
I just ask user to type trait definition in dialog window. Then I compile trait by evaluation of given definition.

Internal Representation and Key Implementation Points.

    Instance Variables
	package:		<Package>
	classTag:		<Symcol>

"
Class {
	#name : 'ClyCreateTraitCommand',
	#superclass : 'ClyBrowserCommand',
	#instVars : [
		'package',
		'packageTag'
	],
	#category : 'Calypso-SystemPlugins-Traits-Browser',
	#package : 'Calypso-SystemPlugins-Traits-Browser'
}

{ #category : 'testing' }
ClyCreateTraitCommand class >> canBeExecutedInContext: aBrowserContext [
	(super canBeExecutedInContext: aBrowserContext) ifFalse: [ ^false ].

	^aBrowserContext isPackageSelected
]

{ #category : 'execution' }
ClyCreateTraitCommand >> executeRefactoring [

	| traitDefinition resultTrait |
	traitDefinition := ClassDefinitionPrinter fluid traitDefinitionTemplateInPackage: package name tag: packageTag named: #TMyTrait.
	traitDefinition := self morphicUIManager multiLineRequest: 'Define trait:' initialAnswer: traitDefinition answerHeight: 250.
	traitDefinition isEmptyOrNil ifTrue: [ ^ self ].
	resultTrait := self browser compileANewClassFrom: traitDefinition notifying: nil startingFrom: nil.
	resultTrait ifNotNil: [ self browser selectClass: resultTrait ]
]

{ #category : 'accessing' }
ClyCreateTraitCommand >> iconName [
	^#trait
]

{ #category : 'testing' }
ClyCreateTraitCommand >> isAvailableInMenuOf: aToolContext [

	^ aToolContext isFullBrowserClassContext
]

{ #category : 'accessing' }
ClyCreateTraitCommand >> name [
	^'New trait'
]

{ #category : 'accessing' }
ClyCreateTraitCommand >> order [

	^ 3
]

{ #category : 'execution' }
ClyCreateTraitCommand >> prepareFullExecution [

	super prepareFullExecution.
	package := self context lastSelectedPackage.
	self context isPackageTagSelected ifTrue: [ packageTag := self context lastSelectedPackageTag ]
]
