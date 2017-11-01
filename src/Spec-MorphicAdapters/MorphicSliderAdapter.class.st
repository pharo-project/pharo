"
I am the adapter used to bridget a SliderPresenter and a PluggableSliderMorph
"
Class {
	#name : #MorphicSliderAdapter,
	#superclass : #AbstractMorphicAdapter,
	#category : 'Spec-MorphicAdapters'
}

{ #category : #'widget API' }
MorphicSliderAdapter >> absoluteValue [

	^ self model absoluteValue
]

{ #category : #'widget API' }
MorphicSliderAdapter >> absoluteValue: aFloat [

	^ self model absoluteValue: aFloat
]

{ #category : #factory }
MorphicSliderAdapter >> buildWidget [
	^ PluggableSliderMorph new
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
		yourself
]

{ #category : #'widget API' }
MorphicSliderAdapter >> label [

	^ self model label
]

{ #category : #'widget API' }
MorphicSliderAdapter >> max [

	^ self model max
]

{ #category : #'widget API' }
MorphicSliderAdapter >> min [

	^ self model min
]

{ #category : #'widget API' }
MorphicSliderAdapter >> quantum [

	^ self model quantum
]

{ #category : #'widget API' }
MorphicSliderAdapter >> value [

	^ self model value
]

{ #category : #'widget API' }
MorphicSliderAdapter >> value: aValue [

	^ self model value: aValue
]
