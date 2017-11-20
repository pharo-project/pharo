"
I am an adapter to ease the bridge a CheckBoxPresenter and a CheckboxMorph
"
Class {
	#name : #MorphicCheckBoxAdapter,
	#superclass : #AbstractMorphicAdapter,
	#category : #Spec-MorphicAdapters
}

{ #category : #factory }
MorphicCheckBoxAdapter >> buildWidget [

	| checkButton |
	checkButton := CheckboxMorph
						on: self 
						selected: #state
						changeSelected: #state:. 			
	checkButton 
			label:	self label; 
			beCheckbox;
			hResizing: #spaceFill; 
			vResizing: #shrinkWrap; 
			setBalloonText: self help;
			getEnabledSelector: #enabled;
			dragEnabled: self dragEnabled;
			dropEnabled: self dropEnabled;	
			labelClickable: self labelClickable.
	^ checkButton
]

{ #category : #'widget API' }
MorphicCheckBoxAdapter >> label [
	
	^ self model label
]

{ #category : #'widget API' }
MorphicCheckBoxAdapter >> labelClickable [
	
	^ self model labelClickable
]

{ #category : #'spec protocol' }
MorphicCheckBoxAdapter >> labelOnLeft [

	^ self widgetDo: [ :w | w listDirection: #leftToRight ]
]

{ #category : #'spec protocol' }
MorphicCheckBoxAdapter >> labelOnRight [

	^ self widgetDo: [ :w | w listDirection: #rightToLeft ]
]

{ #category : #'widget API' }
MorphicCheckBoxAdapter >> state [
	
	^ self model state
]

{ #category : #'widget API' }
MorphicCheckBoxAdapter >> state: aBoolean [
	
	^ self model state: aBoolean
]
