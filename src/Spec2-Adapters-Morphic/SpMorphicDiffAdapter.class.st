"
I am an adpater to bridge a DiffPresenter and a DiffMorph
"
Class {
	#name : #SpMorphicDiffAdapter,
	#superclass : #SpAbstractMorphicAdapter,
	#category : #'Spec2-Adapters-Morphic-Base'
}

{ #category : #factory }
SpMorphicDiffAdapter >> buildWidget [

	^ DiffMorph new  
		on: self ;
		from: self leftText to: self rightText contextClass: self contextClass;
		 hResizing:	#spaceFill;
		vResizing: #spaceFill;
		setBalloonText: self  help;
		showOptions: self showOptions;
		leftLabel: self leftLabel rightLabel: self rightLabel;
		showOnlyDestination: self showOnlyDestination;
		yourself
]

{ #category : #'widget API' }
SpMorphicDiffAdapter >> contextClass [

	^ self model contextClass
]

{ #category : #'spec protocol' }
SpMorphicDiffAdapter >> contextClass: aClass [
	self widgetDo: [ :w | 
		w 
			contextClass: aClass;
			updateText ]
]

{ #category : #'widget API' }
SpMorphicDiffAdapter >> leftLabel [

	^ self model leftLabel
]

{ #category : #'spec protocol' }
SpMorphicDiffAdapter >> leftLabel: aText [

	self widgetDo: [ :w | 
		w leftLabel: aText rightLabel: self rightLabel ]
]

{ #category : #'widget API' }
SpMorphicDiffAdapter >> leftText [

	^ self model leftText
]

{ #category : #'spec protocol' }
SpMorphicDiffAdapter >> leftText: aText [

	self widgetDo: [ :w | 
		w sourceTextModel setText: aText.
		w updateText ]
]

{ #category : #'widget API' }
SpMorphicDiffAdapter >> rightLabel [

	^ self model rightLabel
]

{ #category : #'spec protocol' }
SpMorphicDiffAdapter >> rightLabel: aText [

	self widgetDo: [ :w | 
		w leftLabel: self leftLabel rightLabel: aText ]
]

{ #category : #'widget API' }
SpMorphicDiffAdapter >> rightText [

	^ self model rightText
]

{ #category : #'spec protocol' }
SpMorphicDiffAdapter >> rightText: aText [
	self
		widgetDo: [ :w | 
			w destTextModel setText: aText.
			w updateText ]
]

{ #category : #'spec protocol' }
SpMorphicDiffAdapter >> showBoth [

	self widgetDo: [ :w | w showBoth ]
]

{ #category : #'widget API' }
SpMorphicDiffAdapter >> showOnlyDestination [

	^ self model showOnlyDestination
]

{ #category : #'spec protocol' }
SpMorphicDiffAdapter >> showOnlyDestination: aBoolean [

	self widgetDo: [ :w | w showOnlyDestination: aBoolean ]
]

{ #category : #'widget API' }
SpMorphicDiffAdapter >> showOnlySource [

	^ self model showOnlySource
]

{ #category : #'spec protocol' }
SpMorphicDiffAdapter >> showOnlySource: aBoolean [

	self widgetDo: [ :w | w showOnlySource: aBoolean ]
]

{ #category : #'widget API' }
SpMorphicDiffAdapter >> showOptions [

	^ self model showOptions
]

{ #category : #'spec protocol' }
SpMorphicDiffAdapter >> showOptions: aBoolean [

	self widgetDo: [ :w | w showOptions: aBoolean ]
]
