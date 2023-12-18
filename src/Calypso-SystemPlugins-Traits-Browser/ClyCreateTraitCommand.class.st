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

{ #category : 'activation' }
ClyCreateTraitCommand class >> fullBrowserMenuActivation [
	<classAnnotation>

	^CmdContextMenuActivation byRootGroupItemOrder: 3 for: ClyFullBrowserClassContext
]

{ #category : 'accessing' }
ClyCreateTraitCommand >> defaultMenuIconName [
	^#trait
]

{ #category : 'accessing' }
ClyCreateTraitCommand >> defaultMenuItemName [
	^'New trait'
]

{ #category : 'command execution' }
ClyCreateTraitCommand >> execute [
	| traitDefinition category resultTrait |
	category := package name.
	packageTag ifNotNil: [ category := category , '-' , packageTag ].
	traitDefinition := ClassDefinitionPrinter fluid traitDefinitionTemplateInPackage: category.
	traitDefinition := UIManager default
		                   multiLineRequest: 'Define trait:'
		                   initialAnswer: traitDefinition
		                   answerHeight: 250.
	traitDefinition isEmptyOrNil ifTrue: [ ^ self ].
	resultTrait := browser
		               compileANewClassFrom: traitDefinition
		               notifying: nil
		               startingFrom: nil.
	resultTrait ifNotNil: [ browser selectClass: resultTrait ]
]

{ #category : 'execution' }
ClyCreateTraitCommand >> prepareFullExecutionInContext: aBrowserContext [
	super prepareFullExecutionInContext: aBrowserContext.

	package := aBrowserContext lastSelectedPackage.
	aBrowserContext isPackageTagSelected ifTrue: [
		packageTag := aBrowserContext lastSelectedPackageTag]
]
