Class {
	#name : #ComposablePresenterWithModelTest,
	#superclass : #TestCase,
	#category : #'Spec-Tests-Core'
}

{ #category : #testing }
ComposablePresenterWithModelTest >> testInstanceCreation [

	| aPoint presenter |
	
	aPoint := 40@54.
	presenter := TestingComposablePresenterWithModel on: aPoint.
	
	self assert: presenter model equals: aPoint.
	self deny: (aPoint dependents includes: presenter).
	
	
	
]

{ #category : #testing }
ComposablePresenterWithModelTest >> testInstanceCreationWithValueHolder [

	| point valueHolder presenter |
	
	point := (40@54).
	valueHolder := point asValueHolder.
	presenter := TestingComposablePresenterWithModel on: valueHolder.
	
	self assert: presenter model equals: point.
	
	
	
	
]

{ #category : #testing }
ComposablePresenterWithModelTest >> testModelSettingModelToModel [
	"we had a Model, new model is another Model"

	| model model2 presenter |
	model := TestingPointModel x: 1 y: 2.
	model2 := TestingPointModel x: 4 y: 5.
	presenter := TestingComposablePresenterWithModel on: model.
	self assert: presenter announcingObject isAnnouncingObject.
	self deny: presenter announcingObject isValueHolder.
	self assert: model2 isAnnouncingObject.
	self deny: model2 isValueHolder.
	presenter model: model2.
	self assert: presenter announcingObject isAnnouncingObject.
	self deny: presenter announcingObject isValueHolder.
	self assert: presenter announcingObject identicalTo: model2.
	self assert: presenter announcingObject announcer numberOfSubscriptions > 0
]

{ #category : #testing }
ComposablePresenterWithModelTest >> testModelSettingModelToValueHolder [
	| point model presenter |
	"we had value holder, new model is a model"
	point := 40 @ 54.

	presenter := TestingComposablePresenterWithModel on: point.
	model := TestingPointModel x: 1 y: 2.
	self assert: presenter announcingObject isAnnouncingObject.
	self assert: presenter announcingObject isValueHolder.
	self assert: model isAnnouncingObject.
	self deny: model isValueHolder.
	presenter model: model.
	self assert: presenter announcingObject isAnnouncingObject.
	self deny: presenter announcingObject isValueHolder.
	self assert: presenter announcingObject identicalTo: model.
	self assert: presenter announcingObject announcer numberOfSubscriptions > 0
]

{ #category : #testing }
ComposablePresenterWithModelTest >> testModelSettingObjectToModel [
	| point model presenter |
	"we had a Model, new model is a regular object"
	model := TestingPointModel x: 1 y: 2.
	point := 40 @ 54.
	presenter := TestingComposablePresenterWithModel on: model.
	self assert: presenter announcingObject isAnnouncingObject.
	self deny: presenter announcingObject isValueHolder.
	self deny: point isAnnouncingObject.
	self deny: point isValueHolder.
	presenter model: point.
	self assert: presenter announcingObject isAnnouncingObject.
	self assert: presenter announcingObject isValueHolder.
	self assert: presenter model identicalTo: point.
	self deny: presenter announcingObject identicalTo: model.
	self assert: presenter announcingObject announcer numberOfSubscriptions > 0
]

{ #category : #testing }
ComposablePresenterWithModelTest >> testModelSettingObjectToValueHolder [
	| point point2 presenter |
	"we had value holder, new model is a regular object"
	point := 40 @ 54.
	point2 := 1 @ 0.
	presenter := TestingComposablePresenterWithModel on: point.
	self assert: presenter announcingObject isAnnouncingObject.
	self assert: presenter announcingObject isValueHolder.
	self deny: point2 isAnnouncingObject.
	self deny: point2 isValueHolder.
	presenter model: point2.
	self assert: presenter announcingObject isAnnouncingObject.
	self assert: presenter announcingObject isValueHolder.
	self assert: presenter model identicalTo: point2.
	self assert: presenter announcingObject announcer numberOfSubscriptions > 0
]

{ #category : #testing }
ComposablePresenterWithModelTest >> testModelSettingValueHolderToModel [
	"we had a Model, new model is a value holder"

	| model point valueHolder presenter |
	point := 1 @ 0.
	model := TestingPointModel x: 1 y: 2.
	valueHolder := NewValueHolder value: point.
	presenter := TestingComposablePresenterWithModel on: model.
	self assert: presenter announcingObject isAnnouncingObject.
	self deny: presenter announcingObject isValueHolder.
	self assert: valueHolder isAnnouncingObject.
	self assert: valueHolder isValueHolder.
	presenter model: valueHolder.
	self assert: presenter announcingObject isAnnouncingObject.
	self assert: presenter announcingObject isValueHolder.
	self assert: presenter announcingObject identicalTo: valueHolder.
	self assert: presenter announcingObject announcer numberOfSubscriptions > 0
]

{ #category : #testing }
ComposablePresenterWithModelTest >> testModelSettingValueHolderToValueHolder [
	| point point2 valueHolder presenter |
	"we had value holder, new model is a regular object"
	point := 40 @ 54.
	point2 := 1 @ 0.

	"we had value holder, new model is a value holder"
	presenter := TestingComposablePresenterWithModel on: point.
	valueHolder := NewValueHolder value: point2.
	self assert: presenter announcingObject isAnnouncingObject.
	self assert: presenter announcingObject isValueHolder.
	self deny: point2 isAnnouncingObject.
	self assert: valueHolder isValueHolder.
	presenter model: valueHolder.
	self assert: presenter announcingObject isAnnouncingObject.
	self assert: presenter announcingObject isValueHolder.
	self assert: presenter model identicalTo: point2.
	self assert: presenter announcingObject identicalTo: valueHolder.
	self assert: presenter announcingObject announcer numberOfSubscriptions > 0
]

{ #category : #testing }
ComposablePresenterWithModelTest >> testUpdateModel [
	| aPoint presenter |
	aPoint := TestingPointModel x: 40 y: 54.
	presenter := TestingComposablePresenterWithModel on: aPoint.

	self assert: presenter x text equals: aPoint x asString.
	self assert: presenter y text equals: aPoint y asString.

	aPoint
		x: 1;
		y: 2.
	aPoint valueChanged.

	"the point itself is not subscribed to the presenter"

	self assert: presenter x text equals: aPoint x asString.
	self assert: presenter y text equals: aPoint y asString
]

{ #category : #testing }
ComposablePresenterWithModelTest >> testUpdateModelWithValueHolder [
	| aPoint aValueHolder presenter anAnnouncer |
	"create a point and a value holder that contains it and can react on announcements"
	aPoint := 40 @ 54.
	aValueHolder := NewValueHolder value: aPoint.

	self assert: aValueHolder value identicalTo: aPoint.
	anAnnouncer := aValueHolder announcer.
	self assert: aValueHolder announcer subscriptions subscriptions size equals: 0.

	presenter := TestingComposablePresenterWithModel on: aValueHolder.

	self assert: anAnnouncer identicalTo: aValueHolder announcer.
	self assert: presenter x text equals: aPoint x asString.
	self assert: presenter y text equals: aPoint y asString.

	aPoint setX: 1 setY: 2.
	self assert: aValueHolder value identicalTo: aPoint.
	aValueHolder valueChanged.
	self assert: anAnnouncer identicalTo: aValueHolder announcer.

	"the point itself is not subscribed to the presenter"

	self assert: presenter x text equals: aPoint x asString.
	self assert: presenter y text equals: aPoint y asString
]
