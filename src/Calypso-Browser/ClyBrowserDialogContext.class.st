"
I represent a context of specific dialog which can be opened from the specific browser.
For example there is ClyBrowserSearchDialog. It opens from the browser to allow user choose specific item from the given environment query.
I allow to bind special context implementation to this kind of dialog which can be different depending on the browser.
For example system browser require special ClySystemSearchDialogContext which implement system related context interface.

Following example shows how to use me to annotate specific kind of browser dialog context:
	ClySystemSearchDialogContext class>>selectionStrategy
		<classAnnotation>
		^ClyContextSelectionStrategy for: (ClyBrowserDialogContext of: ClyBrowserSearchDialog in: ClyFullBrowser)

Internal Representation and Key Implementation Points.

    Instance Variables
	browserClass:		<ClyBrowser class>
	dialogClass:		<Class>
"
Class {
	#name : #ClyBrowserDialogContext,
	#superclass : #AnnotationContext,
	#instVars : [
		'dialogClass',
		'browserClass'
	],
	#category : #'Calypso-Browser-Contexts'
}

{ #category : #'instance creation' }
ClyBrowserDialogContext class >> of: aDialogClass in: aBrowserClass [
	^self new 
		dialogClass: aDialogClass;
		browserClass: aBrowserClass
]

{ #category : #'instance creation' }
ClyBrowserDialogContext class >> ofSearchIn: aBrowserClass [
	^self of: ClyBrowserSearchDialogWindow in: aBrowserClass
]

{ #category : #comparing }
ClyBrowserDialogContext >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject
		ifTrue: [ ^ true ].
	self class = anObject class
		ifFalse: [ ^ false ].
	^ dialogClass = anObject dialogClass
		and: [ browserClass = anObject browserClass ]
]

{ #category : #accessing }
ClyBrowserDialogContext >> browserClass [
	^ browserClass
]

{ #category : #accessing }
ClyBrowserDialogContext >> browserClass: anObject [
	browserClass := anObject
]

{ #category : #testing }
ClyBrowserDialogContext >> describes: aBrowserDialog [

	^(aBrowserDialog isKindOf: dialogClass)
		and: [ aBrowserDialog browser isKindOf: browserClass]
]

{ #category : #accessing }
ClyBrowserDialogContext >> dialogClass [
	^ dialogClass
]

{ #category : #accessing }
ClyBrowserDialogContext >> dialogClass: anObject [
	dialogClass := anObject
]

{ #category : #comparing }
ClyBrowserDialogContext >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^ dialogClass hash bitXor: browserClass hash
]
