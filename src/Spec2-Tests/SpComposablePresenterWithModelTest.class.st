Class {
	#name : #SpComposablePresenterWithModelTest,
	#superclass : #TestCase,
	#category : #'Spec2-Tests-Core'
}

{ #category : #testing }
SpComposablePresenterWithModelTest >> testInstanceCreation [

	| aPoint presenter |
	
	aPoint := 40@54.
	presenter := SpTestingPresenterWithModel on: aPoint.
	
	self assert: presenter model equals: aPoint.
	self deny: (aPoint dependents includes: presenter).
	
	
	
]

{ #category : #testing }
SpComposablePresenterWithModelTest >> testInstanceCreationWithValueHolder [

	| point valueHolder presenter |
	
	point := (40@54).
	valueHolder := point asValueHolder.
	presenter := SpTestingPresenterWithModel on: valueHolder.
	
	self assert: presenter model equals: point.
	
	
	
	
]

{ #category : #testing }
SpComposablePresenterWithModelTest >> testModelSettingModelToModel [

	"we had a Model, new model is another Model"
	| model model2 presenter |
	model := SpTestingPointModel x: 1 y: 2.
	model2 := SpTestingPointModel x: 4 y: 5.
	presenter := SpTestingPresenterWithModel on: model.
	self assert: presenter announcingObject isSpAnnouncingObject.
	self deny: presenter announcingObject isValueHolder.
	self assert: model2 isSpAnnouncingObject.
	self deny: model2 isValueHolder.
	presenter model: model2.
	self assert: presenter announcingObject isSpAnnouncingObject.
	self deny: presenter announcingObject isValueHolder.
	self assert: presenter announcingObject == model2.
	self assert: presenter announcingObject announcer numberOfSubscriptions > 0.

]

{ #category : #testing }
SpComposablePresenterWithModelTest >> testModelSettingModelToValueHolder [

	| point model presenter |
	
	"we had value holder, new model is a model"
	point := 40@54.

	presenter := SpTestingPresenterWithModel on: point.
	model := SpTestingPointModel x: 1 y: 2.
	self assert: presenter announcingObject isSpAnnouncingObject.
	self assert: presenter announcingObject isValueHolder.
	self assert: model isSpAnnouncingObject.
	self deny: model isValueHolder.
	presenter model: model.
	self assert: presenter announcingObject isSpAnnouncingObject.
	self deny: presenter announcingObject isValueHolder.
	self assert: presenter announcingObject == model.
	self assert: presenter announcingObject announcer numberOfSubscriptions > 0.


]

{ #category : #testing }
SpComposablePresenterWithModelTest >> testModelSettingObjectToModel [

	| point model presenter |
	
	"we had a Model, new model is a regular object"
	model := SpTestingPointModel x: 1 y: 2.
	point := 40@54.
	presenter := SpTestingPresenterWithModel on: model.
	self assert: presenter announcingObject isSpAnnouncingObject.
	self deny: presenter announcingObject isValueHolder.
	self deny: point isSpAnnouncingObject.
	self deny: point isValueHolder.
	presenter model: point.
	self assert: presenter announcingObject isSpAnnouncingObject.
	self assert: presenter announcingObject isValueHolder.
	self assert: presenter model == point.
	self deny: presenter announcingObject == model.
	self assert: presenter announcingObject announcer numberOfSubscriptions > 0.


]

{ #category : #testing }
SpComposablePresenterWithModelTest >> testModelSettingObjectToValueHolder [

	| point point2 presenter |
	
	"we had value holder, new model is a regular object"
	point := 40@54.
	point2 := 1@0.
	presenter := SpTestingPresenterWithModel on: point.
	self assert: presenter announcingObject isSpAnnouncingObject.
	self assert: presenter announcingObject isValueHolder.
	self deny: point2 isSpAnnouncingObject.
	self deny: point2 isValueHolder.
	presenter model: point2.
	self assert: presenter announcingObject isSpAnnouncingObject.
	self assert: presenter announcingObject isValueHolder.
	self assert: presenter model == point2.
	self assert: presenter announcingObject announcer numberOfSubscriptions > 0.

]

{ #category : #testing }
SpComposablePresenterWithModelTest >> testModelSettingValueHolderToModel [

	"we had a Model, new model is a value holder"
	| model point valueHolder presenter |
	point := 1@0.
	model := SpTestingPointModel x: 1 y: 2.
	valueHolder := NewValueHolder value: point.
	presenter := SpTestingPresenterWithModel on: model.
	self assert: presenter announcingObject isSpAnnouncingObject.
	self deny: presenter announcingObject isValueHolder.
	self assert: valueHolder isSpAnnouncingObject.
	self assert: valueHolder isValueHolder.
	presenter model: valueHolder.
	self assert: presenter announcingObject isSpAnnouncingObject.
	self assert: presenter announcingObject isValueHolder.
	self assert: presenter announcingObject == valueHolder.
	self assert: presenter announcingObject announcer numberOfSubscriptions > 0.

]

{ #category : #testing }
SpComposablePresenterWithModelTest >> testModelSettingValueHolderToValueHolder [

	| point point2 valueHolder presenter |
	
	"we had value holder, new model is a regular object"
	point := 40@54.
	point2 := 1@0.
	
	"we had value holder, new model is a value holder"
	presenter := SpTestingPresenterWithModel on: point.
	valueHolder := NewValueHolder value: point2.
	self assert: presenter announcingObject isSpAnnouncingObject.
	self assert: presenter announcingObject isValueHolder.
	self deny: point2 isSpAnnouncingObject.
	self assert: valueHolder isValueHolder.
	presenter model: valueHolder.
	self assert: presenter announcingObject isSpAnnouncingObject.
	self assert: presenter announcingObject isValueHolder.
	self assert: presenter model == point2.
	self assert: presenter announcingObject == valueHolder.
	self assert: presenter announcingObject announcer numberOfSubscriptions > 0.

]

{ #category : #testing }
SpComposablePresenterWithModelTest >> testUpdateModel [

	| aPoint presenter |
	
	aPoint := SpTestingPointModel x: 40 y: 54.
	presenter := SpTestingPresenterWithModel on: aPoint.
	
	self assert: presenter x text equals: aPoint x asString.
	self assert: presenter y text equals: aPoint y asString.
	
	aPoint x: 1; y: 2.
	aPoint valueChanged.
	
	"the point itself is not subscribed to the presenter"

	self assert: presenter x text equals: aPoint x asString.
	self assert: presenter y text equals: aPoint y asString.
	
	
	
]

{ #category : #testing }
SpComposablePresenterWithModelTest >> testUpdateModelWithValueHolder [

	| aPoint aValueHolder presenter anAnnouncer |
	
	"create a point and a value holder that contains it and can react on announcements"
	aPoint := 40@54.
	aValueHolder := NewValueHolder value: aPoint.
	
	self assert: (aValueHolder value == aPoint).
	anAnnouncer := aValueHolder announcer.
	self assert: (aValueHolder announcer subscriptions subscriptions size) equals: 0. 
	
	presenter := SpTestingPresenterWithModel on: aValueHolder.

	self assert: (anAnnouncer == aValueHolder announcer).
	self assert: presenter x text equals: aPoint x asString.
	self assert: presenter y text equals: aPoint y asString.
	
	aPoint setX: 1 setY: 2.
	self assert: (aValueHolder value == aPoint). 
	aValueHolder valueChanged.
	self assert: (anAnnouncer == aValueHolder announcer).
	
	"the point itself is not subscribed to the presenter"

	self assert: presenter x text equals: aPoint x asString.
	self assert: presenter y text equals: aPoint y asString.
	
	
	
]
