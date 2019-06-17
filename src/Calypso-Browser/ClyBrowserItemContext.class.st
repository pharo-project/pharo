"
I represent any browser context of the selected items of particular class.

I was introduced to activate browser commands and tools in the context of concrete type of selected items. It avoids duplication of annotaions for every browser type. So you can annotated command in context of ClyMethod. And it will be available in all browser which shows methods.

You can use the scope class directly as context in browser annotations:
	ClyRenameMessageCommand class>>shortcutActivation
		^CmdShortcutCommandActivation by: $r meta for: ClyMethod

Internal Representation and Key Implementation Points.

    Instance Variables
	itemType:		<Class>
"
Class {
	#name : #ClyBrowserItemContext,
	#superclass : #AnnotationContext,
	#instVars : [
		'itemType'
	],
	#category : #'Calypso-Browser-Contexts'
}

{ #category : #'instance creation' }
ClyBrowserItemContext class >> itemType: scopeClass [
	^self new 
		itemType: scopeClass
]

{ #category : #comparing }
ClyBrowserItemContext >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject
		ifTrue: [ ^ true ].
	self class = anObject class
		ifFalse: [ ^ false ].
	^ itemType = anObject itemType
]

{ #category : #testing }
ClyBrowserItemContext >> describes: aBrowserContext [

	^aBrowserContext isAboutBrowserItemType: itemType
]

{ #category : #comparing }
ClyBrowserItemContext >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^ itemType hash
]

{ #category : #accessing }
ClyBrowserItemContext >> itemType [
	^ itemType
]

{ #category : #accessing }
ClyBrowserItemContext >> itemType: aClass [
	itemType := aClass
]
