"
I am the conversion of class annotations of a command into one statement of a <calypsoCommands> declaration method, made by ClyCommandDeclarationGenerator.
I know the anchor and the package of the method I go into, the interaction and the menu group I add to, the expression which adds the command, such as 'add: SycRemoveMethodCommand key: $x meta', and the extra messages sent to the new entry, such as 'priority: 10000'.
Usually I convert one annotation. I convert several when an annotation inherited by all the concrete subclasses of an abstract command becomes one #addAllConcreteSubclassesOf:.
"
Class {
	#name : 'ClyAnnotationConversion',
	#superclass : 'Object',
	#instVars : [
		'anchor',
		'packageName',
		'interaction',
		'group',
		'expression',
		'extras',
		'annotations'
	],
	#category : 'Calypso-Commands',
	#package : 'Calypso-Commands'
}

{ #category : 'accessing' }
ClyAnnotationConversion >> anchor [

	^ anchor
]

{ #category : 'accessing' }
ClyAnnotationConversion >> anchor: aClass [

	anchor := aClass
]

{ #category : 'accessing' }
ClyAnnotationConversion >> annotations [

	^ annotations
]

{ #category : 'accessing' }
ClyAnnotationConversion >> annotations: aCollection [

	annotations := aCollection
]

{ #category : 'accessing' }
ClyAnnotationConversion >> expression [

	^ expression
]

{ #category : 'accessing' }
ClyAnnotationConversion >> expression: aString [

	expression := aString
]

{ #category : 'accessing' }
ClyAnnotationConversion >> extras [

	^ extras
]

{ #category : 'accessing' }
ClyAnnotationConversion >> extras: aCollectionOfStrings [

	extras := aCollectionOfStrings
]

{ #category : 'accessing' }
ClyAnnotationConversion >> group [

	^ group
]

{ #category : 'accessing' }
ClyAnnotationConversion >> group: aMenuGroupClass [

	group := aMenuGroupClass
]

{ #category : 'source' }
ClyAnnotationConversion >> hasExtras [

	^ extras isNotEmpty
]

{ #category : 'accessing' }
ClyAnnotationConversion >> interaction [

	^ interaction
]

{ #category : 'accessing' }
ClyAnnotationConversion >> interaction: aSymbol [

	interaction := aSymbol
]

{ #category : 'accessing' }
ClyAnnotationConversion >> packageName [

	^ packageName
]

{ #category : 'accessing' }
ClyAnnotationConversion >> packageName: aString [

	packageName := aString
]

{ #category : 'source' }
ClyAnnotationConversion >> receiverSource [
	"The expression answering the interaction, or the view of its menu group, that the statement adds to."

	| interactionSource |
	interactionSource := 'aCommandMap ' , interaction.
	group = ClyRootMenuGroup ifTrue: [ ^ interactionSource ].
	^ '(' , interactionSource , ' group: ' , group name , ')'
]

{ #category : 'source' }
ClyAnnotationConversion >> statementSource [
	"A statement adding my command alone, with the extra messages sent to its entry."

	^ '(' , self receiverSource , ' ' , expression , ') ' , ('; ' join: extras)
]
