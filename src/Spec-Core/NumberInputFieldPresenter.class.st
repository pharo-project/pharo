"
I'm a text field specialised on numbers. 

I can accept number type (integer or float) and a range (min, max). 
"
Class {
	#name : #NumberInputFieldPresenter,
	#superclass : #TextInputFieldPresenter,
	#instVars : [
		'numberType',
		'minimum',
		'maximum',
		'climbRate',
		'digits'
	],
	#category : #'Spec-Core-Widgets'
}

{ #category : #specs }
NumberInputFieldPresenter class >> adapterName [

	^ #NumberInputFieldAdapter
]

{ #category : #accessing }
NumberInputFieldPresenter class >> defaultClimbRate [

	^ 1
]

{ #category : #accessing }
NumberInputFieldPresenter class >> defaultDigits [

	^ 2
]

{ #category : #accessing }
NumberInputFieldPresenter >> beFloat [

	self numberType: Float
]

{ #category : #accessing }
NumberInputFieldPresenter >> beInteger [

	self numberType: Integer
]

{ #category : #accessing }
NumberInputFieldPresenter >> climbRate [
	"This field indicates the jump rate when a spin is present."

	^ climbRate value
]

{ #category : #accessing }
NumberInputFieldPresenter >> climbRate: aNumber [

	climbRate value: aNumber
]

{ #category : #accessing }
NumberInputFieldPresenter >> digits [
	"digits to show when we have a float number"

	^ digits value
]

{ #category : #accessing }
NumberInputFieldPresenter >> digits: aNumber [
	"digits to show when we have a float number"

	digits value: aNumber
]

{ #category : #initialization }
NumberInputFieldPresenter >> initialize [

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
NumberInputFieldPresenter >> isFloat [ 

	^ self numberType = Float
]

{ #category : #testing }
NumberInputFieldPresenter >> isInteger [ 

	^ self numberType = Integer
]

{ #category : #accessing }
NumberInputFieldPresenter >> maximum [

	^ maximum value
]

{ #category : #accessing }
NumberInputFieldPresenter >> maximum: aNumber [

	maximum value: aNumber
]

{ #category : #accessing }
NumberInputFieldPresenter >> minimum [

	^ minimum value
]

{ #category : #accessing }
NumberInputFieldPresenter >> minimum: aNumber [

	minimum value: aNumber
]

{ #category : #accessing }
NumberInputFieldPresenter >> number [

	^ self text asNumber
]

{ #category : #accessing }
NumberInputFieldPresenter >> number: aNumber [

	self text: aNumber asString

]

{ #category : #accessing }
NumberInputFieldPresenter >> numberType [

	^ numberType value
]

{ #category : #private }
NumberInputFieldPresenter >> numberType: aClass [
	"This can be Integer or Float, no more"

	numberType := aClass
]

{ #category : #accessing }
NumberInputFieldPresenter >> rangeMinimum: minNumber maximum: maxNumber [ 

	self minimum: minNumber.
	self maximum: maxNumber
]
