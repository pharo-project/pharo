"
I'm a text field specialised on numbers. 

I can accept number type (integer or float) and a range (min, max). 
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

	^ climbRate
]

{ #category : #accessing }
SpNumberInputFieldPresenter >> climbRate: aNumber [
	climbRate := aNumber
]

{ #category : #accessing }
SpNumberInputFieldPresenter >> digits [
	"digits to show when we have a float number"

	^ digits
]

{ #category : #accessing }
SpNumberInputFieldPresenter >> digits: aNumber [
	"digits to show when we have a float number"

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

	^ self numberType = Float
]

{ #category : #testing }
SpNumberInputFieldPresenter >> isInteger [ 

	^ self numberType = Integer
]

{ #category : #accessing }
SpNumberInputFieldPresenter >> maximum [
	^ maximum
]

{ #category : #accessing }
SpNumberInputFieldPresenter >> maximum: aNumber [
	maximum := aNumber
]

{ #category : #accessing }
SpNumberInputFieldPresenter >> minimum [
	^ minimum
]

{ #category : #accessing }
SpNumberInputFieldPresenter >> minimum: aNumber [
	minimum := aNumber
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
	^ numberType
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

{ #category : #accessing }
SpNumberInputFieldPresenter >> whenClimbRateChangedDo: aBlock [
	self property: #climbRate whenChangedDo: aBlock
]

{ #category : #accessing }
SpNumberInputFieldPresenter >> whenDigitsChangedDo: aBlock [
	self property: #digits whenChangedDo: aBlock
]

{ #category : #accessing }
SpNumberInputFieldPresenter >> whenMaximumChangedDo: aBlock [
	self property: #maximum whenChangedDo: aBlock
]

{ #category : #accessing }
SpNumberInputFieldPresenter >> whenMinimumChangedDo: aBlock [
	self property: #minimum whenChangedDo: aBlock
]

{ #category : #accessing }
SpNumberInputFieldPresenter >> whenNumberChangedDo: aBlock [
	self whenTextChangedDo: [ :txt | aBlock value: self number ]
]

{ #category : #accessing }
SpNumberInputFieldPresenter >> whenNumberTypeChangedDo: aBlock [
	self property: #numberType whenChangedDo: aBlock
]
