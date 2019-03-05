"
I'm an adapter for Box Layouts.
A paned layout will split (horizontally or vertically) panels (in fixed yet proportional spaces).
"
Class {
	#name : #MorphicBoxAdapter,
	#superclass : #MorphicLayoutAdapter,
	#category : #'Spec-MorphicAdapters-Layout'
}

{ #category : #private }
MorphicBoxAdapter >> addConstraits: constraints toChild: childMorph [
	"adds constraits by child"
	| theMorph |
	
	theMorph := childMorph.
	constraints isExpand 
		ifTrue: [ 
			"if is not 'fill', in fact I need to pad it with another (since it 'expands', 
			 but does not 'fills')"
			constraints isFill ifFalse: [ 
				theMorph
					hResizing: #rigid;
					vResizing: #rigid.
				theMorph := self newPanel
					addMorphBack: theMorph;
					yourself ]. 
			"Set to fill"
			theMorph 
				hResizing: #spaceFill;
				vResizing: #spaceFill ]
		ifFalse: [
			"Set morph to stay rigid"
			self flag: #TODO. "This works for vertical but has to be inversed for horizontal"
			theMorph 
				hResizing: #spaceFill; 
				vResizing: #rigid.
			constraints width ifNotNil: [ :w | theMorph width: w ].
			constraints height ifNotNil: [ :h | theMorph height: h ] ].
	"constraints padding"
	
	^ theMorph 
]

{ #category : #private }
MorphicBoxAdapter >> basicAdd: aPresenter constraints: constraints to: aPanel [
	| childMorph |

	childMorph := self buildMorph: aPresenter constraints: constraints.	
	constraints isPlacedAtStart
		ifTrue: [ aPanel submorphs first addMorphBack: childMorph ]
		ifFalse: [ aPanel submorphs second addMorphBack: childMorph ].
		
	^ childMorph
]

{ #category : #factory }
MorphicBoxAdapter >> layout: aLayout [

	"build of widget was differed up to here (to know what is the direction)"
	widget := aLayout direction newWidgetOn: self.
	widget borderWidth: aLayout borderWidth.
	"the spacing needs to be applied to the inner sub-panels"
	widget submorphs do: [ :each |
		each cellInset: aLayout spacing ].
	"widget homogeneous: aLayout isHomogeneous"
]

{ #category : #factory }
MorphicBoxAdapter >> newHorizontal [

	^ self newPanel 
		listDirection: #leftToRight;
		addMorphBack: (self newPanel
			listDirection: #leftToRight;
			yourself);
		addMorphBack: (self newPanel
			listDirection: #leftToRight;
			hResizing: #shrinkWrap;
			width: 0;
			yourself);
		yourself	
]

{ #category : #factory }
MorphicBoxAdapter >> newPanel [

	^ PanelMorph new 
		changeTableLayout;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		color: Color transparent;
		borderColor: Color transparent;
		yourself
]

{ #category : #factory }
MorphicBoxAdapter >> newVertical [

	^ self newPanel 
		listDirection: #topToBottom;
		addMorphBack: (self newPanel
			listDirection: #topToBottom;
			yourself);
		addMorphBack: (self newPanel
			listDirection: #topToBottom;
			vResizing: #shrinkWrap;
			height: 0;
			yourself);
		yourself	
]
