"
I am an abstract class providing all the properties shared amongs all the stub adapters. They do not produce real views, only stub objects.

"
Class {
	#name : #SpecStubAbstractAdapter,
	#superclass : #AbstractAdapter,
	#category : #'Spec-StubAdapter-Adapters'
}

{ #category : #accessing }
SpecStubAbstractAdapter class >> adaptingName [
	"Override me if adapter will not follow the common pattern"

	"remove 'SpecStub' part and *in general*, I will have the adapting name"

	^ (self name allButFirst: 8) asSymbol
]

{ #category : #accessing }
SpecStubAbstractAdapter class >> allAdapters [

	^ self allSubclasses
]

{ #category : #protocol }
SpecStubAbstractAdapter >> add: aWidget [
]

{ #category : #factory }
SpecStubAbstractAdapter >> addPresenterIn: widgetToBuild withSpecLayout: aSpec [
	
	
]

{ #category : #compatibility }
SpecStubAbstractAdapter >> bindKeyCombination: aShortcut toAction: aBlock [


]

{ #category : #initialization }
SpecStubAbstractAdapter >> borderColor: color [
]

{ #category : #initialization }
SpecStubAbstractAdapter >> borderWidth: width [
]

{ #category : #factory }
SpecStubAbstractAdapter >> buildWidget [

	^ self class new
]

{ #category : #initialization }
SpecStubAbstractAdapter >> color: color [
]

{ #category : #initialization }
SpecStubAbstractAdapter >> dragEnabled: aBoolean [
]

{ #category : #initialization }
SpecStubAbstractAdapter >> dropEnabled: aBoolean [
]

{ #category : #initialization }
SpecStubAbstractAdapter >> enabled: aBoolean [
]

{ #category : #protocol }
SpecStubAbstractAdapter >> hRigid [
]

{ #category : #protocol }
SpecStubAbstractAdapter >> hShrinkWrap [

]

{ #category : #protocol }
SpecStubAbstractAdapter >> hSpaceFill [
]

{ #category : #protocol }
SpecStubAbstractAdapter >> layout: aLayout [
]

{ #category : #protocol }
SpecStubAbstractAdapter >> removeSubWidgets [
]

{ #category : #initialization }
SpecStubAbstractAdapter >> setBalloonText: aString [
]

{ #category : #factory }
SpecStubAbstractAdapter >> takeKeyboardFocus [
]

{ #category : #factory }
SpecStubAbstractAdapter >> useProportionalLayout [
]

{ #category : #protocol }
SpecStubAbstractAdapter >> vRigid [
]

{ #category : #protocol }
SpecStubAbstractAdapter >> vShrinkWrap [
]

{ #category : #protocol }
SpecStubAbstractAdapter >> vSpaceFill [
]

{ #category : #protocol }
SpecStubAbstractAdapter >> when: anAnnouncement do: aBlock [
]
