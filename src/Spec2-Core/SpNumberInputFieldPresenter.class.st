"
A presenter  for a  text field specialised on numbers. 
`SpNumberInputFieldPresenter` accepts a number type (integer or float) and a range (min, max). 

"
Class {
	#name : #SpNumberInputFieldPresenter,
	#superclass : #SpTextInputFieldPresenter,
	#instVars : [
		'#numberType => SpObservableSlot',
		'#minimum => SpObservableSlot',
		'#maximum => SpObservableSlot',
		'#climbRate => SpObservableSlot',
		'#digits => SpObservableSlot'
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

{ #category : #documentation }
SpNumberInputFieldPresenter class >> documentFactoryMethodSelector [

	^ #newNumberInput
]

{ #category : #api }
SpNumberInputFieldPresenter >> beFloat [
	"Set the presenter to display float numbers"

	self numberType: Float
]

{ #category : #api }
SpNumberInputFieldPresenter >> beInteger [
	"Set the presenter to display integer numbers"

	self numberType: Integer
]

{ #category : #api }
SpNumberInputFieldPresenter >> climbRate [
	"Answer the jump rate when a spin is present."

	^ climbRate
]

{ #category : #api }
SpNumberInputFieldPresenter >> climbRate: aNumber [
	"Set the jump rate when a spin is present."

	climbRate := aNumber
]

{ #category : #api }
SpNumberInputFieldPresenter >> digits [
	"Answer digits to show when we have a float number"

	^ digits
]

{ #category : #api }
SpNumberInputFieldPresenter >> digits: aNumber [
	"Set digits to show when we have a float number"

	digits := aNumber
]

{ #category : #initialization }
SpNumberInputFieldPresenter >> initialize [

	super initialize.
	self beInteger.
	self digits: self class defaultDigits.
	self climbRate: self class defaultClimbRate.
]

{ #category : #testing }
SpNumberInputFieldPresenter >> isFloat [ 
	"Answer if the number type is a float."

	^ self numberType = Float
]

{ #category : #testing }
SpNumberInputFieldPresenter >> isInteger [ 
	"Answer if the number type is an integer."

	^ self numberType = Integer
]

{ #category : #api }
SpNumberInputFieldPresenter >> maximum [
	"Answer the maximum number to accept (it can be nil, if not maximum is set)"

	^ maximum
]

{ #category : #api }
SpNumberInputFieldPresenter >> maximum: aNumber [
	"Set the maximum number to accept"

	maximum := aNumber
]

{ #category : #api }
SpNumberInputFieldPresenter >> minimum [
	"Answer the minimum number to accept (it can be nil, if not minimum is set)"

	^ minimum
]

{ #category : #api }
SpNumberInputFieldPresenter >> minimum: aNumber [
	"Answer the minimum number to accept."

	minimum := aNumber
]

{ #category : #api }
SpNumberInputFieldPresenter >> number [
	"Answer the number ingressed."

	^ self text trimmed 
		ifNotEmpty: [ :aString | aString asNumber ]
		ifEmpty: [ 0 ]
]

{ #category : #api }
SpNumberInputFieldPresenter >> number: aNumber [
	"Set the number to display."

	self text: aNumber asString

]

{ #category : #api }
SpNumberInputFieldPresenter >> numberType [
	"Answer the type of number the input field will show (Integer or Float)"

	^ numberType
]

{ #category : #private }
SpNumberInputFieldPresenter >> numberType: aClass [
	"This can be Integer or Float, no more"

	numberType := aClass
]

{ #category : #api }
SpNumberInputFieldPresenter >> rangeMinimum: minNumber maximum: maxNumber [ 
	"A convenience method to set the maximum and minimum values to accept."

	self minimum: minNumber.
	self maximum: maxNumber
]

{ #category : #'api-events' }
SpNumberInputFieldPresenter >> whenClimbRateChangedDo: aBlock [
	"Inform when climbRate property has changed. 
	 `aBlock` has three optional arguments: 
	 - new value
	 - old value
	 - the announcement triggering this action"

	self property: #climbRate whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpNumberInputFieldPresenter >> whenDigitsChangedDo: aBlock [
	"Inform when digits property state has changed. 
	 `aBlock` has three optional arguments: 
	 - new value
	 - old value
	 - the announcement triggering this action"

	self property: #digits whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpNumberInputFieldPresenter >> whenMaximumChangedDo: aBlock [
	"Inform when maximum property has changed. 
	 `aBlock` has three optional arguments: 
	 - new value
	 - old value
	 - the announcement triggering this action"

	self property: #maximum whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpNumberInputFieldPresenter >> whenMinimumChangedDo: aBlock [
	"Inform when minimum property has changed. 
	 `aBlock` has three optional arguments: 
	 - new value
	 - old value
	 - the announcement triggering this action"

	self property: #minimum whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpNumberInputFieldPresenter >> whenNumberChangedDo: aBlock [
	"Inform when number property has changed. 
	 `aBlock` has three optional arguments: 
	 - new value
	 - old value
	 - the announcement triggering this action"

	self whenTextChangedDo: [ :txt | aBlock value: self number ]
]

{ #category : #'api-events' }
SpNumberInputFieldPresenter >> whenNumberTypeChangedDo: aBlock [
	"Inform when numberType property has changed. 
	 `aBlock` has three optional arguments: 
	 - new value
	 - old value
	 - the announcement triggering this action"

	self property: #numberType whenChangedDo: aBlock
]
