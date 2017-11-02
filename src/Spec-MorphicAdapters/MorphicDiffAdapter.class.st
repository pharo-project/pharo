"
I am an adpater to bridge a DiffPresenter and a DiffMorph
"
Class {
	#name : #MorphicDiffAdapter,
	#superclass : #AbstractMorphicAdapter,
	#category : 'Spec-MorphicAdapters'
}

{ #category : #factory }
MorphicDiffAdapter >> buildWidget [

	^ DiffMorph new  
		on: self ;
		from: self leftText to: self rightText contextClass: self contextClass;
		 hResizing:	#spaceFill;
		vResizing: #spaceFill;
		setBalloonText: self  help;
		showOptions: self showOptions;
		showOnlyDestination: self showOnlyDestination;
		yourself
]

{ #category : #'widget API' }
MorphicDiffAdapter >> contextClass [

	^ self model contextClass
]

{ #category : #'spec protocol' }
MorphicDiffAdapter >> contextClass: aClass [
	self widgetDo: [ :w | 
		w 
			contextClass: aClass;
			updateText ]
]

{ #category : #'widget API' }
MorphicDiffAdapter >> leftText [

	^ self model leftText
]

{ #category : #'spec protocol' }
MorphicDiffAdapter >> leftText: aText [

	self widgetDo: [ :w | 
		w sourceTextModel setText: aText.
		w updateText ]
]

{ #category : #'widget API' }
MorphicDiffAdapter >> rightText [

	^ self model rightText
]

{ #category : #'spec protocol' }
MorphicDiffAdapter >> rightText: aText [
	self
		widgetDo: [ :w | 
			w destTextModel setText: aText.
			w updateText ]
]

{ #category : #'spec protocol' }
MorphicDiffAdapter >> showBoth [

	self widgetDo: [ :w | w showBoth ]
]

{ #category : #'widget API' }
MorphicDiffAdapter >> showOnlyDestination [

	^ self model showOnlyDestination
]

{ #category : #'spec protocol' }
MorphicDiffAdapter >> showOnlyDestination: aBoolean [

	self widgetDo: [ :w | w showOnlyDestination: aBoolean ]
]

{ #category : #'widget API' }
MorphicDiffAdapter >> showOnlySource [

	^ self model showOnlySource
]

{ #category : #'spec protocol' }
MorphicDiffAdapter >> showOnlySource: aBoolean [

	self widgetDo: [ :w | w showOnlySource: aBoolean ]
]

{ #category : #'widget API' }
MorphicDiffAdapter >> showOptions [

	^ self model showOptions
]

{ #category : #'spec protocol' }
MorphicDiffAdapter >> showOptions: aBoolean [

	self widgetDo: [ :w | w showOptions: aBoolean ]
]
