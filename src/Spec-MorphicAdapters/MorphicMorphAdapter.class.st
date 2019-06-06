Class {
	#name : #MorphicMorphAdapter,
	#superclass : #AbstractMorphicAdapter,
	#category : #'Spec-MorphicAdapters-Base'
}

{ #category : #factory }
MorphicMorphAdapter >> buildWidget [
	| panel |

	panel := PanelMorph new 
		changeTableLayout;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		color: Color transparent;
		borderColor: Color transparent;
		yourself.
		
	self presenter morph 
		ifNotNil: [ :morph | panel addMorphBack: morph ].
	self presenter 
		whenMorphChangedDo: [ :morph | 
			panel 
				removeAllMorphs;
				addMorphBack: morph ].
	
	^ panel
]
