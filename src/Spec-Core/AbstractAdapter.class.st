"
I am an abstract class.

I'm the superclass of all the adapters used to link a Spec widget presenter to a framework specific widget (by example ButtonPresenter <-> PluggableButtonMorph).

The current implementation installs my instances as dependent of the presenter and my changed: method propagates updates to the widget I create (via my buildWidget method). This implementation is not optimal. 

In the future my instances should just be responsible to create a widget and install all the communication between the presenter and the widget. I should not be a middle man. 
"
Class {
	#name : #AbstractAdapter,
	#superclass : #Model,
	#instVars : [
		'model',
		'widget',
		'selector'
	],
	#category : #'Spec-Core'
}

{ #category : #'instance creation' }
AbstractAdapter class >> adapt: aComposablePresenter [
	
	^ self new
		adapt: aComposablePresenter;
		yourself
]

{ #category : #initialization }
AbstractAdapter >> adapt: aComposablePresenter [

	model := aComposablePresenter.
	aComposablePresenter addDependent: self.

	widget := self buildWidget.
]

{ #category : #protocol }
AbstractAdapter >> add: aWidget [

	self subclassResponsibility
]

{ #category : #protocol }
AbstractAdapter >> asWidget [

	^ self widget
]

{ #category : #factory }
AbstractAdapter >> buildWidget [

	^ self subclassResponsibility
]

{ #category : #protocol }
AbstractAdapter >> hRigid [

	self subclassResponsibility
]

{ #category : #protocol }
AbstractAdapter >> hShrinkWrap [

	self subclassResponsibility
]

{ #category : #protocol }
AbstractAdapter >> hSpaceFill [

	self subclassResponsibility
]

{ #category : #protocol }
AbstractAdapter >> isRedrawable [
	"This must be overriden in the adapter representing your container"
	
	^ false
]

{ #category : #testing }
AbstractAdapter >> isSpecAdapter [

	^ true
]

{ #category : #protocol }
AbstractAdapter >> layout: aLayout [

	self subclassResponsibility
]

{ #category : #accessing }
AbstractAdapter >> model [
	^ model
]

{ #category : #protocol }
AbstractAdapter >> removeSubWidgets [

	self subclassResponsibility
]

{ #category : #accessing }
AbstractAdapter >> selector [
	
	^ selector
]

{ #category : #accessing }
AbstractAdapter >> selector: anObject [
	
	selector := anObject
]

{ #category : #protocol }
AbstractAdapter >> takeKeyboardFocus [
	
	self widget ifNotNil: [:w | w takeKeyboardFocus ]
]

{ #category : #updating }
AbstractAdapter >> update: aSymbol [

	self changed: aSymbol
]

{ #category : #updating }
AbstractAdapter >> update: aSymbol with: anArray [

	self perform: aSymbol withArguments: anArray
]

{ #category : #protocol }
AbstractAdapter >> useProportionalLayout [

	self subclassResponsibility
]

{ #category : #protocol }
AbstractAdapter >> vRigid [

	self subclassResponsibility
]

{ #category : #protocol }
AbstractAdapter >> vShrinkWrap [

	self subclassResponsibility
]

{ #category : #protocol }
AbstractAdapter >> vSpaceFill [

	self subclassResponsibility
]

{ #category : #protocol }
AbstractAdapter >> when: anAnnouncement do: aBlock [

	self subclassResponsibility
]

{ #category : #accessing }
AbstractAdapter >> widget [
	^ widget
]

{ #category : #private }
AbstractAdapter >> widgetDo: aBlock [

	^ self widget ifNotNil: aBlock
]
