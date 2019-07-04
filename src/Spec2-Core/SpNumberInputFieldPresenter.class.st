"
I'm a text field specialised on numbers. 

I can accept number type (integer or float) and a range (min, max). 
"
Class {
	#name : #SpNumberInputFieldPresenter,
	#superclass : #SpTextInputFieldPresenter,
	#instVars : [
		'numberType',
		'minimum',
		'maximum',
		'climbRate',
		'digits'
	],
	#category : #'Spec2-Core-Widgets'
}

{ #category : #specs }
SpNumberInputFieldPresenter class >> adapterName [

	^ #NumberInputFieldAdapter
]

{ #category : #accessing }
SpNumberInputFieldPresenter class >> defaultClimbRate [

	^ 1
]

{ #category : #accessing }
SpNumberInputFieldPresenter class >> defaultDigits [

	^ 2
]

{ #category : #accessing }
SpNumberInputFieldPresenter >> beFloat [

	self numberType: Float
]

{ #category : #accessing }
SpNumberInputFieldPresenter >> beInteger [

	self numberType: Integer
]

{ #category : #accessing }
SpNumberInputFieldPresenter >> climbRate [
	"This field indicates the jump rate when a spin is present."

	^ climbRate value
]

{ #category : #accessing }
SpNumberInputFieldPresenter >> climbRate: aNumber [

	climbRate value: aNumber
]

{ #category : #accessing }
SpNumberInputFieldPresenter >> digits [
	"digits to show when we have a float number"

	^ digits value
]

{ #category : #accessing }
SpNumberInputFieldPresenter >> digits: aNumber [
	"digits to show when we have a float number"

	digits value: aNumber
]

{ #category : #initialization }
SpNumberInputFieldPresenter >> initialize [

	super initialize.

	numberType := nil asValueHolder. 
	minimum := nil asValueHolder. 
	maximum := nil asValueHolder. 
	climbRate := nil asValueHolder. 
	digits := nil asValueHolder.
	
	self beInteger.
	self digits: self class defaultDigits.
	self climbRate: self class defaultClimbRate.
]

{ #category : #testing }
SpNumberInputFieldPresenter >> isFloat [ 

	^ self numberType = Float
]

{ #category : #testing }
SpNumberInputFieldPresenter >> isInteger [ 

	^ self numberType = Integer
]

{ #category : #accessing }
SpNumberInputFieldPresenter >> maximum [

	^ maximum value
]

{ #category : #accessing }
SpNumberInputFieldPresenter >> maximum: aNumber [

	maximum value: aNumber
]

{ #category : #accessing }
SpNumberInputFieldPresenter >> minimum [

	^ minimum value
]

{ #category : #accessing }
SpNumberInputFieldPresenter >> minimum: aNumber [

	minimum value: aNumber
]

{ #category : #accessing }
SpNumberInputFieldPresenter >> number [

	^ self text asNumber
]

{ #category : #accessing }
SpNumberInputFieldPresenter >> number: aNumber [

	self text: aNumber asString

]

{ #category : #accessing }
SpNumberInputFieldPresenter >> numberType [

	^ numberType value
]

{ #category : #private }
SpNumberInputFieldPresenter >> numberType: aClass [
	"This can be Integer or Float, no more"

	numberType := aClass
]

{ #category : #accessing }
SpNumberInputFieldPresenter >> rangeMinimum: minNumber maximum: maxNumber [ 

	self minimum: minNumber.
	self maximum: maxNumber
]
