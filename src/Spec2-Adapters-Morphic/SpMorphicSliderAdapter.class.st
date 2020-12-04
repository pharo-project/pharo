"
I am the adapter used to bridget a SliderPresenter and a PluggableSliderMorph
"
Class {
	#name : #SpMorphicSliderAdapter,
	#superclass : #SpAbstractMorphicAdapter,
	#category : #'Spec2-Adapters-Morphic-Base'
}

{ #category : #'widget API' }
SpMorphicSliderAdapter >> absoluteValue [

	^ self presenter absoluteValue
]

{ #category : #'widget API' }
SpMorphicSliderAdapter >> absoluteValue: aFloat [

	^ self presenter absoluteValue: aFloat
]

{ #category : #factory }
SpMorphicSliderAdapter >> buildWidget [

	| preWidget |
	preWidget := PluggableSliderMorph new
		             model: self;
		             getValueSelector: #value;
		             setValueSelector: #value:;
		             value: self absoluteValue;
		             getLabelSelector: #label;
		             max: self max;
		             min: self min;
		             quantum: self quantum;
		             setBalloonText: self help;
		             vResizing: #spaceFill;
		             hResizing: #spaceFill;
		             yourself.
	self presenter isHorizontal ifFalse: [ 
		preWidget := TransformationMorph new asFlexOf: preWidget.
		preWidget transform withAngle: 90 degreesToRadians negated ].

	self presenter whenMinChangedDo: [ :newValue | 
		preWidget min: newValue ].
	self presenter whenMaxChangedDo: [ :newValue | 
		preWidget max: newValue ].
	self presenter whenQuantumChangedDo: [ :newValue | 
		preWidget quantum: newValue ].
	self presenter whenValueChangedDo: [ :newValue | 
		preWidget value: newValue ].

	^ preWidget
]

{ #category : #'widget API' }
SpMorphicSliderAdapter >> label [

	^ self presenter label
]

{ #category : #'widget API' }
SpMorphicSliderAdapter >> max [

	^ self presenter max
]

{ #category : #'widget API' }
SpMorphicSliderAdapter >> min [

	^ self presenter min
]

{ #category : #'widget API' }
SpMorphicSliderAdapter >> quantum [

	^ self model quantum
]

{ #category : #'widget API' }
SpMorphicSliderAdapter >> value [
	^ self presenter value
]

{ #category : #'widget API' }
SpMorphicSliderAdapter >> value: aValue [

	| value |	
	value := aValue isNumber
		ifTrue: [ aValue ] 
		ifFalse: [ 
			(aValue includes: $/)
				ifTrue: [ (NumberParser on: aValue) nextFraction ]
				ifFalse: [ aValue asNumber ] ].

	^ self presenter value: value asFloat
]
