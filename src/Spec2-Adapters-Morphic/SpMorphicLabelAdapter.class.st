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

	aMorph fitContents.
	super applyStyle: aMorph
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
		setBalloonText: self getHelpText;
		setProperty: #minWidth toValue: 3. "3 is the default value of StringMorph. We do not want the default value of LabelMorph that is the min width of the content..."
		
	^ label
]

{ #category : #private }
SpMorphicLabelAdapter >> getHelpText [

	^ self presenter help ifNotNil: [ :aString | aString localizedForPresenter: self presenter ]
]

{ #category : #'model access' }
SpMorphicLabelAdapter >> getText [

	^self presenter label ifNotNil: [ :aString | aString localizedForPresenter: self presenter ]
]

{ #category : #accessing }
SpMorphicLabelAdapter >> label [
	
	^ widget contents
]
