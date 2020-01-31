Class {
	#name : #SpMorphicToggleButtonAdapter,
	#superclass : #SpAbstractMorphicAdapter,
	#category : #'Spec2-Adapters-Morphic-Base'
}

{ #category : #factory }
SpMorphicToggleButtonAdapter >> buildWidget [
	| checkButton |

	checkButton := PluggableToggleButtonMorph
		on: self 
		getState: #state
		action:  #toggleAction:
		label: #label 
		menu: nil. 	
	
	checkButton 
		icon: self icon;
		label:	 self label; 
		hResizing: #spaceFill; 
		vResizing: #shrinkWrap; 
		setBalloonText: self help;
		getEnabledSelector: #enabled;
		dragEnabled: self dragEnabled;
		dropEnabled: self dropEnabled.
			
	self presenter whenLabelChangedDo: [ :newLabel | checkButton label: newLabel ].
	self presenter whenIconChangedDo: [ checkButton icon: self icon ].
	self presenter whenChangedDo: [ checkButton pressed: self model state ].

	^ checkButton
]

{ #category : #emulating }
SpMorphicToggleButtonAdapter >> clicked [
	
	widget performAction.
	self model state: widget isPressed
]

{ #category : #accessing }
SpMorphicToggleButtonAdapter >> helpText [
	
	^ widget balloonText
]

{ #category : #'widget API' }
SpMorphicToggleButtonAdapter >> icon [
	
	^ self presenter icon
]

{ #category : #'widget API' }
SpMorphicToggleButtonAdapter >> label [
	
	^ self presenter label
]

{ #category : #'widget API' }
SpMorphicToggleButtonAdapter >> state [
	
	^ self presenter state
]

{ #category : #'widget API' }
SpMorphicToggleButtonAdapter >> state: aBoolean [
	
	^ self presenter state: aBoolean
]

{ #category : #events }
SpMorphicToggleButtonAdapter >> toggleAction: aBoolean [ 
	
	self model state: aBoolean.
	^ self model action cull: aBoolean 
]
