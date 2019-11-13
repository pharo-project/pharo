"
A SliderPresenter is a spec presenter for a slider.
See AbstractWidgetPresenter

self example

I provide the following variables and their accessors
- max is the maximum of the slide.
- min is the minimum of the slide.
- value is the value of the slide, it is between the maximum and the minimum
- absoluteValue is the value of the slide in a percent of the maximum.
- label is the label in the background.

I provide the following methods
- color: to set the background color, it must be called after have displayed the widget.

todo
- quantum

"
Class {
	#name : #SpSliderPresenter,
	#superclass : #SpAbstractWidgetPresenter,
	#instVars : [
		'#max => SpObservableSlot',
		'#min => SpObservableSlot',
		'#quantum => SpObservableSlot',
		'#value => SpObservableSlot',
		'#absoluteValue => SpObservableSlot',
		'#isHorizontal',
		'#label => SpObservableSlot',
		'#marks => SpObservableSlot'
	],
	#category : #'Spec2-Core-Widgets'
}

{ #category : #specs }
SpSliderPresenter class >> adapterName [

	^ #SliderAdapter
]

{ #category : #api }
SpSliderPresenter >> absoluteValue [
	"Return the position of the slider in a scale between 0 and 1 despite of the min and max value"

	^ absoluteValue
]

{ #category : #api }
SpSliderPresenter >> absoluteValue: aFloat [
	"Set the position of the slider in a scale between 0 and 1 despite of the min and max value"

	absoluteValue := aFloat
]

{ #category : #initialization }
SpSliderPresenter >> absoluteValueToValue: v [
	^ self min + (v * (self max - self min)) roundTo: self quantum
]

{ #category : #api }
SpSliderPresenter >> addMark: aText at: aValue [
	^ self
		marks:
			(self marks
				add:
					(SpSliderMark new
						value: aValue;
						text: aText;
						yourself);
				yourself)
]

{ #category : #api }
SpSliderPresenter >> beHorizontal [
	isHorizontal := true
]

{ #category : #api }
SpSliderPresenter >> beVertical [
	isHorizontal := false
]

{ #category : #api }
SpSliderPresenter >> color: aColor [

	"Hack because during the interpretation, the state is slightly inconistent"
	self widget ifNotNil: [:w | w == self ifFalse: [ super color: aColor ]]
]

{ #category : #initialization }
SpSliderPresenter >> initialize [
	| isChanging |
	super initialize.

	isHorizontal := true.
	marks := OrderedCollection new.
	max := 100.
	min := 0.
	quantum := 1.
	value := 0.
	absoluteValue := 0.
	label := ''.
	isChanging := false.

	self whenMaxChangedDo: [ :m | self changed: {#max: . m} ].
	self whenMinChangedDo: [ :m | self changed: {#min: . m} ].
	self whenQuantumChangedDo: [ :m | self changed: {#quantum: . m} ].
	self
		whenValueChangedDo: [ :v | 
			isChanging
				ifFalse: [ isChanging := true.
					self absoluteValue: (self valueToAbsoluteValue: v).
					self changed: {#scaledValue: . v}.
					isChanging := false ].
			self changed: #value ].
	self
		whenAbsoluteValueChangedDo: [ :v | 
			isChanging
				ifFalse: [ isChanging := true.
					self value: (self absoluteValueToValue: v).
					self changed: {#value: . v}.
					isChanging := false ] ].
	self whenLabelChangedDo: [ :v | self changed: #label ]
]

{ #category : #api }
SpSliderPresenter >> isHorizontal [
	^ isHorizontal
]

{ #category : #api }
SpSliderPresenter >> isVertical [
	^ self isHorizontal not
]

{ #category : #api }
SpSliderPresenter >> label [
	"Return the label of the slider"

	^ label
]

{ #category : #api }
SpSliderPresenter >> label: aString [
	"Set the label of the slider"

	label := aString
]

{ #category : #api }
SpSliderPresenter >> marks [
	^ marks
]

{ #category : #api }
SpSliderPresenter >> marks: anObject [
	marks := anObject
]

{ #category : #api }
SpSliderPresenter >> max [
	"Return the maximun value"

	^ max
]

{ #category : #api }
SpSliderPresenter >> max: anObject [
	"Set the maximun value"

	max := anObject
]

{ #category : #api }
SpSliderPresenter >> min [
	"Return the minimum value"

	^ min
]

{ #category : #api }
SpSliderPresenter >> min: anObject [
	"Set the minimum value"

	min := anObject
]

{ #category : #api }
SpSliderPresenter >> quantum [
	"Return the quantum betwen values"

	^ quantum
]

{ #category : #api }
SpSliderPresenter >> quantum: aNumber [
	"Set the quantum betwen values"

	quantum := aNumber
]

{ #category : #initialization }
SpSliderPresenter >> reset [
	"Reset the cursor to the minimum value"
	
	self value: self min
]

{ #category : #initialization }
SpSliderPresenter >> scaleValue: v [
	^ ((v - self min) / (self max - self min)) asFloat
]

{ #category : #api }
SpSliderPresenter >> value [
	"Return the current value in a range between min and max"

	^ value
]

{ #category : #api }
SpSliderPresenter >> value: aNumber [
	"Set the value in a range between min and max"

	value = aNumber ifTrue: [ ^ self ].
	value := aNumber
]

{ #category : #initialization }
SpSliderPresenter >> valueToAbsoluteValue: v [
	^ ((v - self min) / (self max - self min)) asFloat
]

{ #category : #'api-events' }
SpSliderPresenter >> whenAbsoluteValueChangedDo: aBlock [
	"Block performed when the absolute value changed"

	self property: #absoluteValue whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpSliderPresenter >> whenLabelChangedDo: aBlock [
	"Block performed when the label changed"

	self property: #label whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpSliderPresenter >> whenMarksChangedDo: aBlock [
	"Block performed when the label changed"

	self property: #marks whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpSliderPresenter >> whenMaxChangedDo: aBlock [ 
	"Block performed when the maximum value changed"

	self property: #max whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpSliderPresenter >> whenMinChangedDo: aBlock [ 
	"Block performed when the minimum value changed"

	self property: #min whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpSliderPresenter >> whenQuantumChangedDo: aBlock [ 
	"Block performed when the quantum value changed"

	self property: #quantum whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpSliderPresenter >> whenValueChangedDo: aBlock [ 
	"Block performed when the value changed"

	self property: #value whenChangedDo: aBlock
]
