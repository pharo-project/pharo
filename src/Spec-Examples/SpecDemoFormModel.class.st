"
Simple structure-like model for the form demo
"
Class {
	#name : #SpecDemoFormModel,
	#superclass : #Object,
	#instVars : [
		'name',
		'surname',
		'number1',
		'number2',
		'scale',
		'password',
		'rememberMe',
		'date',
		'gender',
		'selectedItems'
	],
	#category : #'Spec-Examples-Demo-Forms'
}

{ #category : #accessing }
SpecDemoFormModel >> date [
	^ date
]

{ #category : #accessing }
SpecDemoFormModel >> date: anObject [
	date := anObject
]

{ #category : #accessing }
SpecDemoFormModel >> gender [
	^ gender
]

{ #category : #accessing }
SpecDemoFormModel >> gender: anObject [
	gender := anObject
]

{ #category : #accessing }
SpecDemoFormModel >> genders [
	^ #(male female)
]

{ #category : #initialization }
SpecDemoFormModel >> initialize [

	super initialize.
	
	name := 'some name'.
	surname := ''.
	number1 := 1.
	number2 := 12.
	scale := 42.
	password := 'password'.
	rememberMe := true.
	date := '2018-01-01' asDate.
	gender := #male.
	selectedItems := { self items at: 3. self items at: 4 }
	
]

{ #category : #initialization }
SpecDemoFormModel >> items [

	^ #('item 1' 'item 2' 'item 3' 'item 4' 'item 5')
]

{ #category : #accessing }
SpecDemoFormModel >> name [
	^ name
]

{ #category : #accessing }
SpecDemoFormModel >> name: anObject [
	name := anObject
]

{ #category : #accessing }
SpecDemoFormModel >> number1 [
	^ number1
]

{ #category : #accessing }
SpecDemoFormModel >> number1: anObject [
	number1 := anObject
]

{ #category : #accessing }
SpecDemoFormModel >> number2 [
	^ number2
]

{ #category : #accessing }
SpecDemoFormModel >> number2: anObject [
	number2 := anObject
]

{ #category : #accessing }
SpecDemoFormModel >> password [
	^ password
]

{ #category : #accessing }
SpecDemoFormModel >> password: anObject [
	password := anObject
]

{ #category : #accessing }
SpecDemoFormModel >> rememberMe [
	^ rememberMe
]

{ #category : #accessing }
SpecDemoFormModel >> rememberMe: anObject [
	rememberMe := anObject
]

{ #category : #accessing }
SpecDemoFormModel >> scale [
	^ scale
]

{ #category : #accessing }
SpecDemoFormModel >> scale: anObject [
	scale := anObject
]

{ #category : #accessing }
SpecDemoFormModel >> selectedItems [
	^ selectedItems
]

{ #category : #accessing }
SpecDemoFormModel >> selectedItems: anObject [
	selectedItems := anObject
]

{ #category : #accessing }
SpecDemoFormModel >> surname [
	^ surname
]

{ #category : #accessing }
SpecDemoFormModel >> surname: anObject [
	surname := anObject
]
