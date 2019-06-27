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
		'max',
		'min',
		'quantum',
		'value',
		'isChanging',
		'absoluteValue',
		'labelHolder',
		'marksHolder',
		'isHorizontal'
	],
	#category : #'Spec2-Core-Widgets'
}

{ #category : #specs }
SpSliderPresenter class >> adapterName [

	^ #SliderAdapter
]

{ #category : #api }
SpSliderPresenter >> absoluteValue [
	"<api: #inspect>"
	"Return the position of the slider in a scale between 0 and 1 despite of the min and max value"
	^ absoluteValue value
]

{ #category : #api }
SpSliderPresenter >> absoluteValue: aFloat [
	"<api: #float min: 0 max: 1 getter:#absoluteValue registration: #whenAbsoluteValueChanged:>"
	"Set the position of the slider in a scale between 0 and 1 despite of the min and max value"

	absoluteValue value: aFloat
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
	
	super initialize.
	
	isHorizontal := true.
	marksHolder := OrderedCollection new asValueHolder.
	max := 100 asValueHolder.
	min := 0 asValueHolder.
	quantum := 1 asValueHolder.
	value := 0 asValueHolder.
	absoluteValue := 0 asValueHolder.
	labelHolder := '' asValueHolder.
	isChanging := false.
	
	max whenChangedDo: [ :m | self changed: {#max:. m} ].
	min whenChangedDo: [ :m | self changed: {#min:. m} ].
	quantum whenChangedDo: [ :m | self changed: {#quantum:. m} ].
	value whenChangedDo: [ :v | 
		isChanging ifFalse: [ 
			isChanging := true.
			self absoluteValue: (self valueToAbsoluteValue: v).
			self changed: { #scaledValue:. v}.
			isChanging := false ].
		self changed: #value ].
	absoluteValue whenChangedDo: [ :v | 
		isChanging ifFalse: [ 
			isChanging := true.
			self value: (self absoluteValueToValue: v).
			self changed: { #value:. v}.
			isChanging := false ] ].
	labelHolder whenChangedDo: [ :v | self changed: #label ]
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
	"<api: #inspect>"
	"Return the label of the slider"

	^ labelHolder value
]

{ #category : #api }
SpSliderPresenter >> label: aString [
	"<api: #string getter: #label registration: #whenLabelChanged:>"
	"Set the label of the slider"
	
	labelHolder value: aString
]

{ #category : #api }
SpSliderPresenter >> marks [
	^ marksHolder value
]

{ #category : #api }
SpSliderPresenter >> marks: anObject [

	marksHolder value: anObject
]

{ #category : #api }
SpSliderPresenter >> max [
	"<api: #inspect>"
	"Return the maximun value"
	
	^ max value
]

{ #category : #api }
SpSliderPresenter >> max: anObject [
	"<api: #integer min: 1 max: 100 getter:#max registration: #whenMaxChanged:>"
	"Set the maximun value"

	max value: anObject
]

{ #category : #api }
SpSliderPresenter >> min [
	"<api: #inspect>"
	"Return the minimum value"
	
	^ min value
]

{ #category : #api }
SpSliderPresenter >> min: anObject [
	"<api: #integer min: 0 max: 99 getter:#min registration: #whenMinChanged:>"
	"Set the minimum value"
	
	min value: anObject
]

{ #category : #api }
SpSliderPresenter >> quantum [
	"<api: #inspect>"
	"Return the quantum betwen values"

	^ quantum value
]

{ #category : #api }
SpSliderPresenter >> quantum: aNumber [
	"<api: #integer min: #min max: #max getter:#quantum registration: #whenQuantumChanged:>"
	"Set the quantum betwen values"

	quantum value: aNumber
]

{ #category : #api }
SpSliderPresenter >> reset [
	"<api:#do>"
	"Reset the cursor to the minimum value"
	
	self value: self min
]

{ #category : #initialization }
SpSliderPresenter >> scaleValue: v [
	^ ((v - self min) / (self max - self min)) asFloat
]

{ #category : #api }
SpSliderPresenter >> value [
	"<api: #inspect>"
	"Return the current value in a range between min and max"

	^ value value
]

{ #category : #api }
SpSliderPresenter >> value: aNumber [
	"<api: #integer min: #min max: #max getter:#value registration: #whenValueChanged:>"
	"Set the value in a range between min and max"
	
	value value == aNumber ifTrue: [ ^ self ].
	value value: aNumber.
]

{ #category : #initialization }
SpSliderPresenter >> valueToAbsoluteValue: v [
	^ ((v - self min) / (self max - self min)) asFloat
]

{ #category : #'api-events' }
SpSliderPresenter >> whenAbsoluteValueChangedDo: aBlock [
	"Block performed when the absolute value changed"

	absoluteValue whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpSliderPresenter >> whenLabelChangedDo: aBlock [
	"Block performed when the label changed"

	labelHolder whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpSliderPresenter >> whenMarksChangedDo: aBlock [
	"Block performed when the label changed"

	marksHolder whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpSliderPresenter >> whenMaxChangedDo: aBlock [ 
	"Block performed when the maximum value changed"

	max whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpSliderPresenter >> whenMinChangedDo: aBlock [ 
	"Block performed when the minimum value changed"

	min whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpSliderPresenter >> whenQuantumChangedDo: aBlock [ 
	"Block performed when the quantum value changed"

	quantum whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpSliderPresenter >> whenValueChangedDo: aBlock [ 
	"Block performed when the value changed"

	value whenChangedDo: aBlock
]
