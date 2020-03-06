"
I am the adapter used to bridget a LabelPresenter and a LabelMorph
"
Class {
	#name : #SpMorphicLabelAdapter,
	#superclass : #SpAbstractMorphicAdapter,
	#instVars : [
		'emphasisOptions'
	],
	#category : #'Spec2-Adapters-Morphic-Base'
}

{ #category : #initialization }
SpMorphicLabelAdapter >> applyStyle: aMorph [

	super applyStyle: aMorph.
	aMorph fitContents
]

{ #category : #factory }
SpMorphicLabelAdapter >> buildWidget [
	| label |
	label := LabelMorph new model: self.
	label
		getEnabledSelector: #enabled;
		getTextSelector: #getText;
		vResizing: #shrinkWrap;
		hResizing: #spaceFill;
		dragEnabled: self dragEnabled;
		dropEnabled: self dropEnabled;
		setBalloonText: self help;
		setProperty: #minWidth toValue: 3. "3 is the default value of StringMorph. We do not want the default value of LabelMorph that is the min width of the content..."
		
	^ label
]

{ #category : #'model access' }
SpMorphicLabelAdapter >> getText [

	^ self model label
]

{ #category : #accessing }
SpMorphicLabelAdapter >> label [
	
	^ widget contents
]
