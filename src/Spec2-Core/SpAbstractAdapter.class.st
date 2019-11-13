"
I am an abstract class.

I'm the superclass of all the adapters used to link a Spec widget presenter to a framework specific widget (by example ButtonPresenter <-> PluggableButtonMorph).

The current implementation installs my instances as dependent of the presenter and my changed: method propagates updates to the widget I create (via my buildWidget method). This implementation is not optimal. 

In the future my instances should just be responsible to create a widget and install all the communication between the presenter and the widget. I should not be a middle man. 
"
Class {
	#name : #SpAbstractAdapter,
	#superclass : #Model,
	#instVars : [
		'model',
		'widget',
		'selector',
		'owner'
	],
	#category : #'Spec2-Core-Base'
}

{ #category : #'instance creation' }
SpAbstractAdapter class >> adapt: aPresenter [
	
	^ self new
		adapt: aPresenter;
		yourself
]

{ #category : #accessing }
SpAbstractAdapter class >> adaptingAliases [
	"Answers an array of aliases my class can adapt."

	^ #()
]

{ #category : #accessing }
SpAbstractAdapter class >> adaptingName [
	"Answers the name this component adapts."

	^ self subclassResponsibility
]

{ #category : #accessing }
SpAbstractAdapter class >> allAdapters [
	"The abstract adapters should be able to return all the adapters for a framework"
	
	^ self subclassResponsibility
]

{ #category : #testing }
SpAbstractAdapter class >> isAbstract [
	^ self = SpAbstractAdapter
]

{ #category : #accessing }
SpAbstractAdapter class >> owner: anOwner [

	^ self new owner: anOwner; yourself
]

{ #category : #initialization }
SpAbstractAdapter >> adapt: aPresenter [

	model := aPresenter.
	aPresenter addDependent: self.

	widget := self buildWidget
]

{ #category : #protocol }
SpAbstractAdapter >> add: aWidget [

	self subclassResponsibility
]

{ #category : #factory }
SpAbstractAdapter >> buildWidget [

	^ self subclassResponsibility
]

{ #category : #protocol }
SpAbstractAdapter >> hRigid [

	self subclassResponsibility
]

{ #category : #protocol }
SpAbstractAdapter >> hShrinkWrap [

	self subclassResponsibility
]

{ #category : #protocol }
SpAbstractAdapter >> hSpaceFill [

	self subclassResponsibility
]

{ #category : #testing }
SpAbstractAdapter >> hasWidget: aMorphicButtonAdapter [ 
	
	^ self widget hasWidget: aMorphicButtonAdapter widget
]

{ #category : #protocol }
SpAbstractAdapter >> isRedrawable [
	"This must be overriden in the adapter representing your container"
	
	^ false
]

{ #category : #visibility }
SpAbstractAdapter >> isVisible [
	
	self subclassResponsibility
]

{ #category : #protocol }
SpAbstractAdapter >> layout: aLayout [

	self subclassResponsibility
]

{ #category : #accessing }
SpAbstractAdapter >> model [
	^ model
]

{ #category : #accessing }
SpAbstractAdapter >> owner [

	"Every object instantiated by the SpecInerpreter should have an owner that will assign it to a particular presenter"
	^ owner
]

{ #category : #accessing }
SpAbstractAdapter >> owner: anObject [
	owner := anObject
]

{ #category : #accessing }
SpAbstractAdapter >> presenter [
	^ model
]

{ #category : #protocol }
SpAbstractAdapter >> removeSubWidgets [

	self subclassResponsibility
]

{ #category : #emulating }
SpAbstractAdapter >> rightClicked [

	self subclassResponsibility
]

{ #category : #accessing }
SpAbstractAdapter >> selector [
	
	^ selector
]

{ #category : #accessing }
SpAbstractAdapter >> selector: anObject [
	
	selector := anObject
]

{ #category : #updating }
SpAbstractAdapter >> update: aSymbol [

	self changed: aSymbol
]

{ #category : #updating }
SpAbstractAdapter >> update: aSymbol with: anArray [

	self perform: aSymbol withArguments: anArray
]

{ #category : #protocol }
SpAbstractAdapter >> useProportionalLayout [

	self subclassResponsibility
]

{ #category : #protocol }
SpAbstractAdapter >> vRigid [

	self subclassResponsibility
]

{ #category : #protocol }
SpAbstractAdapter >> vShrinkWrap [

	self subclassResponsibility
]

{ #category : #protocol }
SpAbstractAdapter >> vSpaceFill [

	self subclassResponsibility
]

{ #category : #protocol }
SpAbstractAdapter >> when: anAnnouncement do: aBlock [

	self subclassResponsibility
]

{ #category : #accessing }
SpAbstractAdapter >> widget [
	^ widget
]

{ #category : #private }
SpAbstractAdapter >> widgetDo: aBlock [

	^ self widget ifNotNil: aBlock
]
