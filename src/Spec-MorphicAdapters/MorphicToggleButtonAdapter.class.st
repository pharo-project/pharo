Class {
	#name : #MorphicToggleButtonAdapter,
	#superclass : #AbstractMorphicAdapter,
	#category : #'Spec-MorphicAdapters-Base'
}

{ #category : #factory }
MorphicToggleButtonAdapter >> buildWidget [

	| checkButton |
	checkButton := PluggableToggleButtonMorph
							on: self 
							getState: #state
							action:  #toggleAction:
							label: #label 
							menu: nil. 	
	
	checkButton 
			label:	self label; 
			hResizing: #spaceFill; 
			vResizing: #shrinkWrap; 
			setBalloonText: self help;
			getEnabledSelector: #enabled;
			dragEnabled: self dragEnabled;
			dropEnabled: self dropEnabled.
			
	self presenter whenLabelChangedDo: [ :newLabel | checkButton label: newLabel ].
	self presenter whenChangedDo: [ checkButton pressed: self model state ].
	^ checkButton
]

{ #category : #emulating }
MorphicToggleButtonAdapter >> clicked [
	
	widget performAction.
	self model state: widget isPressed
]

{ #category : #accessing }
MorphicToggleButtonAdapter >> helpText [
	
	^ widget balloonText
]

{ #category : #'widget API' }
MorphicToggleButtonAdapter >> label [
	
	^ self model label
]

{ #category : #'widget API' }
MorphicToggleButtonAdapter >> state [
	
	^ self presenter state
]

{ #category : #'widget API' }
MorphicToggleButtonAdapter >> state: aBoolean [
	
	^ self presenter state: aBoolean
]

{ #category : #events }
MorphicToggleButtonAdapter >> toggleAction: aBoolean [ 
	
	self model state: aBoolean.
	^ self model action cull: aBoolean 
]
