"
I am an abstract class providing all the properties shared amongs all the stub adapters. They do not produce real views, only stub objects.

"
Class {
	#name : #SpStubAbstractAdapter,
	#superclass : #SpAbstractAdapter,
	#category : #'Spec2-Adapters-Stub-Adapters'
}

{ #category : #accessing }
SpStubAbstractAdapter class >> adaptingName [
	"Override me if adapter will not follow the common pattern"

	"remove 'SpStub' part and *in general*, I will have the adapting name"

	^ (self name withoutPrefix: 'SpStub') asSymbol
]

{ #category : #accessing }
SpStubAbstractAdapter class >> allAdapters [

	^ self allSubclasses
]

{ #category : #protocol }
SpStubAbstractAdapter >> add: aWidget [
]

{ #category : #factory }
SpStubAbstractAdapter >> addPresenterIn: widgetToBuild withSpecLayout: aSpec [
	
	
]

{ #category : #compatibility }
SpStubAbstractAdapter >> bindKeyCombination: aShortcut toAction: aBlock [


]

{ #category : #initialization }
SpStubAbstractAdapter >> borderColor: color [
]

{ #category : #initialization }
SpStubAbstractAdapter >> borderWidth: width [
]

{ #category : #factory }
SpStubAbstractAdapter >> buildWidget [

	^ self class new
]

{ #category : #initialization }
SpStubAbstractAdapter >> color: color [
]

{ #category : #initialization }
SpStubAbstractAdapter >> dragEnabled: aBoolean [
]

{ #category : #initialization }
SpStubAbstractAdapter >> dropEnabled: aBoolean [
]

{ #category : #initialization }
SpStubAbstractAdapter >> enabled: aBoolean [
]

{ #category : #protocol }
SpStubAbstractAdapter >> hRigid [
]

{ #category : #protocol }
SpStubAbstractAdapter >> hShrinkWrap [

]

{ #category : #protocol }
SpStubAbstractAdapter >> hSpaceFill [
]

{ #category : #protocol }
SpStubAbstractAdapter >> layout: aLayout [
]

{ #category : #protocol }
SpStubAbstractAdapter >> removeSubWidgets [
]

{ #category : #initialization }
SpStubAbstractAdapter >> setBalloonText: aString [
]

{ #category : #factory }
SpStubAbstractAdapter >> takeKeyboardFocus [
]

{ #category : #factory }
SpStubAbstractAdapter >> useProportionalLayout [
]

{ #category : #protocol }
SpStubAbstractAdapter >> vRigid [
]

{ #category : #protocol }
SpStubAbstractAdapter >> vShrinkWrap [
]

{ #category : #protocol }
SpStubAbstractAdapter >> vSpaceFill [
]

{ #category : #protocol }
SpStubAbstractAdapter >> when: anAnnouncement do: aBlock [
]
