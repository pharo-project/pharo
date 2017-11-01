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
	#name : #SliderPresenter,
	#superclass : #AbstractWidgetPresenter,
	#instVars : [
		'max',
		'min',
		'quantum',
		'value',
		'isSetting',
		'isChanging',
		'absoluteValue',
		'labelHolder'
	],
	#category : 'Spec-Core-Widgets'
}

{ #category : #specs }
SliderPresenter class >> adapterName [

	^ #SliderAdapter
]

{ #category : #specs }
SliderPresenter class >> defaultSpec [
	<spec: #default>
	
	^ #(SliderAdapter
		adapt: #(model))
]

{ #category : #specs }
SliderPresenter class >> example [
	| slider |
	slider := self new.
	slider
		min: 0;
		max: 100;
		whenValueChangedDo: [ :value | slider label: value asString ].
	slider openWithSpec.
	^ slider 
]

{ #category : #api }
SliderPresenter >> absoluteValue [
	"<api: #inspect>"
	"Return the position of the slider in a scale between 0 and 1 despite of the min and max value"
	^ absoluteValue value
]

{ #category : #api }
SliderPresenter >> absoluteValue: aFloat [
	"<api: #float min: 0 max: 1 getter:#absoluteValue registration: #whenAbsoluteValueChanged:>"
	"Set the position of the slider in a scale between 0 and 1 despite of the min and max value"

	absoluteValue value: aFloat
]

{ #category : #api }
SliderPresenter >> color: aColor [

	"Hack because during the interpretation, the state is slightly inconistent"
	self widget ifNotNil: [:w | w == self ifFalse: [ super color: aColor ]]
]

{ #category : #initialization }
SliderPresenter >> initialize [

	super initialize.
	max := 100 asValueHolder.
	min := 0 asValueHolder.
	quantum := 1 asValueHolder.
	value := 0 asValueHolder.
	absoluteValue := 0 asValueHolder.
	labelHolder := '' asValueHolder.
	isChanging := false.
	
	max whenChangedDo: [:m | self changed: { #max: . m } ].
	min whenChangedDo: [:m | self changed: { #min: . m } ].
	quantum whenChangedDo: [:m | self changed: { #quantum: . m } ].
	
	value whenChangedDo: [:v |
		isChanging ifFalse: [
			isChanging := true.
			self absoluteValue: ((v-self min)/(self max-self min)).
			self changed: { #scaledValue: . v }.
			isChanging := false ].
		self changed: #value.].
	
	absoluteValue whenChangedDo: [:v | 
		isChanging ifFalse: [
			isChanging := true.
			self value: (self min+(v*(self max-self min)) roundTo: self quantum).
			self changed: { #value: . v }.
			isChanging := false ]].
	
	labelHolder whenChangedDo: [:v | self changed: #label ].
]

{ #category : #api }
SliderPresenter >> label [
	"<api: #inspect>"
	"Return the label of the slider"

	^ labelHolder value
]

{ #category : #api }
SliderPresenter >> label: aString [
	"<api: #string getter: #label registration: #whenLabelChanged:>"
	"Set the label of the slider"
	
	labelHolder value: aString
]

{ #category : #api }
SliderPresenter >> max [
	"<api: #inspect>"
	"Return the maximun value"
	
	^ max value
]

{ #category : #api }
SliderPresenter >> max: anObject [
	"<api: #integer min: 1 max: 100 getter:#max registration: #whenMaxChanged:>"
	"Set the maximun value"

	max value: anObject
]

{ #category : #api }
SliderPresenter >> min [
	"<api: #inspect>"
	"Return the minimum value"
	
	^ min value
]

{ #category : #api }
SliderPresenter >> min: anObject [
	"<api: #integer min: 0 max: 99 getter:#min registration: #whenMinChanged:>"
	"Set the minimum value"
	
	min value: anObject
]

{ #category : #api }
SliderPresenter >> quantum [
	"<api: #inspect>"
	"Return the quantum betwen values"

	^ quantum value
]

{ #category : #api }
SliderPresenter >> quantum: aNumber [
	"<api: #integer min: #min max: #max getter:#quantum registration: #whenQuantumChanged:>"
	"Set the quantum betwen values"

	quantum value: aNumber
]

{ #category : #api }
SliderPresenter >> reset [
	"<api:#do>"
	"Reset the cursor to the minimum value"
	
	self value: self min
]

{ #category : #api }
SliderPresenter >> value [
	"<api: #inspect>"
	"Return the current value in a range between min and max"

	^ value value
]

{ #category : #api }
SliderPresenter >> value: aNumber [
	"<api: #integer min: #min max: #max getter:#value registration: #whenValueChanged:>"
	"Set the value in a range between min and max"
	
	value value == aNumber ifTrue: [ ^ self ].
	value value: aNumber.
]

{ #category : #api-events }
SliderPresenter >> whenAbsoluteValueChangedDo: aBlock [
	"Block performed when the absolute value changed"

	absoluteValue whenChangedDo: aBlock
]

{ #category : #api-events }
SliderPresenter >> whenLabelChangedDo: aBlock [
	"Block performed when the label changed"

	labelHolder whenChangedDo: aBlock
]

{ #category : #api-events }
SliderPresenter >> whenMaxChangedDo: aBlock [ 
	"Block performed when the maximum value changed"

	max whenChangedDo: aBlock
]

{ #category : #api-events }
SliderPresenter >> whenMinChangedDo: aBlock [ 
	"Block performed when the minimum value changed"

	min whenChangedDo: aBlock
]

{ #category : #api-events }
SliderPresenter >> whenQuantumChangedDo: aBlock [ 
	"Block performed when the quantum value changed"

	quantum whenChangedDo: aBlock
]

{ #category : #api-events }
SliderPresenter >> whenValueChangedDo: aBlock [ 
	"Block performed when the value changed"

	value whenChangedDo: aBlock
]
