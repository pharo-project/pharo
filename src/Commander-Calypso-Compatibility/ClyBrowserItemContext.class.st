"
Deprecated: commands, tabs and table decorators are declared on the item type itself, with the <calypsoCommands> pragma (ClyCommandMap). I am kept, in the deprecated Commander-Calypso-Compatibility package, for the projects which still use me, and I will be removed in Pharo 16. doc/Calypso/CommandAnnotationsMigration.md of the Pharo repository explains the migration.

I represent any browser context of the selected items of particular class.

I was introduced to activate browser tools and table decorators in the context of a concrete type of selected items. It avoids duplicating annotations for every browser type: a tool annotated in the context of ClyClassComment is available in all the browsers which show class comments. Commands no longer use me: they are declared on the item type directly (see ClyCommandMap).

Annotations get me from the item type:
	ClyRichTextClassCommentEditorToolMorph class >> classCommentTabActivation
		<classAnnotation>
		^ ClyTabActivationStrategyAnnotation for: ClyClassComment asCalypsoItemContext

Internal Representation and Key Implementation Points.

    Instance Variables
	itemType:		<Class>
"
Class {
	#name : 'ClyBrowserItemContext',
	#superclass : 'AnnotationContext',
	#instVars : [
		'itemType'
	],
	#category : 'Commander-Calypso-Compatibility-Annotations',
	#package : 'Commander-Calypso-Compatibility',
	#tag : 'Annotations'
}

{ #category : 'instance creation' }
ClyBrowserItemContext class >> itemType: scopeClass [
	^self new
		itemType: scopeClass
]

{ #category : 'comparing' }
ClyBrowserItemContext >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject
		ifTrue: [ ^ true ].
	self class = anObject class
		ifFalse: [ ^ false ].
	^ itemType = anObject itemType
]

{ #category : 'accessing' }
ClyBrowserItemContext >> commandAnchor [
	"The anchor of ClyCommandMap that describes the same contexts as me, used to convert class annotations into declarations."

	^ itemType
]

{ #category : 'testing' }
ClyBrowserItemContext >> describes: aBrowserContext [

	^aBrowserContext isAboutBrowserItemType: itemType
]

{ #category : 'comparing' }
ClyBrowserItemContext >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^ itemType hash
]

{ #category : 'accessing' }
ClyBrowserItemContext >> itemType [
	^ itemType
]

{ #category : 'accessing' }
ClyBrowserItemContext >> itemType: aClass [
	itemType := aClass
]
