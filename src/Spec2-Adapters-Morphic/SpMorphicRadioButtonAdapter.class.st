"
I am the adapter used to link a RadioButtonPresenter with a CheckboxMorph (which can also act as a radio button)
"
Class {
	#name : #SpMorphicRadioButtonAdapter,
	#superclass : #SpAbstractMorphicAdapter,
	#category : #'Spec2-Adapters-Morphic-Base'
}

{ #category : #factory }
SpMorphicRadioButtonAdapter >> buildWidget [
	| radioButton |

	self presenter isInitialStateSet ifFalse: [ 
		"initial state is false, set all associated buttons to false too"
		self presenter property: #state rawValue: false.
		self presenter associatedRadioButtons do: [ :each |
			each property: #state rawValue: false ] ].

	radioButton := CheckboxMorph new
		on: self 
			selected: #state 
			changeSelected: #state:;
		getEnabledSelector: #enabled;
		label: self getLabelText;
		labelClickable: self labelClickable;
		beRadioButton;
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap;
		setBalloonText: self getHelpText;
		dragEnabled: self dragEnabled;
		dropEnabled: self dropEnabled;
		yourself.
		
	self presenter whenLabelChangedDo: [ :newLabel | radioButton label: (newLabel ifNotNil: [ :aString | aString localizedForPresenter: self presenter ]) ].
	self presenter whenChangedDo: [ radioButton updateSelection ].

	^ radioButton
]

{ #category : #simulating }
SpMorphicRadioButtonAdapter >> clicked [
	
	widget toggleSelected
]

{ #category : #factory }
SpMorphicRadioButtonAdapter >> getHelpText [

	^ self presenter help ifNotNil: [ :aString | aString localizedForPresenter: self presenter ]
]

{ #category : #private }
SpMorphicRadioButtonAdapter >> getLabelText [

	^ self presenter label ifNotNil: [ :aString | aString localizedForPresenter: self presenter ]
]

{ #category : #'widget API' }
SpMorphicRadioButtonAdapter >> label [

	^ self getLabelText
]

{ #category : #'widget API' }
SpMorphicRadioButtonAdapter >> labelClickable [

	^ self presenter labelClickable
]

{ #category : #'widget API' }
SpMorphicRadioButtonAdapter >> state [

	^ self presenter state
]

{ #category : #'widget API' }
SpMorphicRadioButtonAdapter >> state: aBoolean [

	^ self presenter state: aBoolean
]
