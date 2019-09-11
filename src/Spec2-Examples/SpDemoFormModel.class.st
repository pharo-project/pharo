"
Simple structure-like model for the form demo
"
Class {
	#name : #SpDemoFormModel,
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
	#category : #'Spec2-Examples-Demo-Forms'
}

{ #category : #accessing }
SpDemoFormModel >> date [
	^ date
]

{ #category : #accessing }
SpDemoFormModel >> date: anObject [
	date := anObject
]

{ #category : #accessing }
SpDemoFormModel >> elements [
	^ { 
		'name' -> self name.
		'surname' -> self surname.
		'number 1' -> self number1.
		'number 2' -> self number2.
		'scale' -> self scale.
		'password' -> self password.
		'rememberMe' -> self rememberMe.
		'date' -> self date.
		'gender' -> self gender.
		'items' -> self selectedItems
		}
]

{ #category : #accessing }
SpDemoFormModel >> gender [
	^ gender
]

{ #category : #accessing }
SpDemoFormModel >> gender: anObject [
	gender := anObject
]

{ #category : #accessing }
SpDemoFormModel >> genders [
	^ #(male female)
]

{ #category : #initialization }
SpDemoFormModel >> initialize [

	super initialize.
	
	name := 'some name'.
	surname := ''.
	number1 := 1.
	number2 := 12.009.
	scale := 42.
	password := 'password'.
	rememberMe := true.
	date := '2018-01-01' asDate.
	gender := #male.
	selectedItems := { self items at: 3. self items at: 4 }
	
]

{ #category : #initialization }
SpDemoFormModel >> items [

	^ #('item 1' 'item 2' 'item 3' 'item 4' 'item 5')
]

{ #category : #accessing }
SpDemoFormModel >> name [
	^ name
]

{ #category : #accessing }
SpDemoFormModel >> name: anObject [
	name := anObject
]

{ #category : #accessing }
SpDemoFormModel >> number1 [
	^ number1
]

{ #category : #accessing }
SpDemoFormModel >> number1: anObject [
	number1 := anObject
]

{ #category : #accessing }
SpDemoFormModel >> number2 [
	^ number2
]

{ #category : #accessing }
SpDemoFormModel >> number2: anObject [
	number2 := anObject
]

{ #category : #accessing }
SpDemoFormModel >> password [
	^ password
]

{ #category : #accessing }
SpDemoFormModel >> password: anObject [
	password := anObject
]

{ #category : #accessing }
SpDemoFormModel >> rememberMe [
	^ rememberMe
]

{ #category : #accessing }
SpDemoFormModel >> rememberMe: anObject [
	rememberMe := anObject
]

{ #category : #accessing }
SpDemoFormModel >> scale [
	^ scale
]

{ #category : #accessing }
SpDemoFormModel >> scale: anObject [
	scale := anObject
]

{ #category : #accessing }
SpDemoFormModel >> selectedItems [
	^ selectedItems
]

{ #category : #accessing }
SpDemoFormModel >> selectedItems: anObject [
	selectedItems := anObject asArray
]

{ #category : #accessing }
SpDemoFormModel >> surname [
	^ surname
]

{ #category : #accessing }
SpDemoFormModel >> surname: anObject [
	surname := anObject
]
