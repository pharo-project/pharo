"
I'm an adapter for Box Layouts.
A paned layout will split (horizontally or vertically) panels (in fixed yet proportional spaces).
"
Class {
	#name : #SpMorphicBoxAdapter,
	#superclass : #SpMorphicLayoutAdapter,
	#instVars : [
		'layout'
	],
	#category : #'Spec2-Adapters-Morphic-Layout'
}

{ #category : #factory }
SpMorphicBoxAdapter >> add: aPresenter [
 
	self add: aPresenter constraints: SpBoxConstraints new
]

{ #category : #private }
SpMorphicBoxAdapter >> addConstraits: constraints toChild: childMorph [
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
			layout direction setRigidityOfNonExpendedMorph: theMorph.
			constraints width ifNotNil: [ :w | theMorph width: w ].
			constraints height ifNotNil: [ :h | theMorph height: h ] ].
	"constraints padding"
	
	^ theMorph 
]

{ #category : #private }
SpMorphicBoxAdapter >> basicAdd: aPresenter constraints: constraints to: aPanel [
	| childMorph |

	childMorph := self buildMorph: aPresenter constraints: constraints.	
	constraints isPlacedAtStart
		ifTrue: [ aPanel submorphs first addMorphBack: childMorph ]
		ifFalse: [ aPanel submorphs second addMorphBack: childMorph ].
		
	^ childMorph
]

{ #category : #accessing }
SpMorphicBoxAdapter >> childrenWidgets [
	
	^ widget submorphs first submorphs, widget submorphs second submorphs
]

{ #category : #factory }
SpMorphicBoxAdapter >> layout: aLayout [

	layout := aLayout.
	
	widget
		ifNil: [
	"build of widget was differed up to here (to know what is the direction)"
	widget := aLayout direction newWidgetOn: self.
	widget borderWidth: aLayout borderWidth.
	"the spacing needs to be applied to the inner sub-panels"
	widget submorphs do: [ :each |
		each cellInset: aLayout spacing ].
	"widget homogeneous: aLayout isHomogeneous" ].
	
	super layout: aLayout
]

{ #category : #factory }
SpMorphicBoxAdapter >> newHorizontal [

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
SpMorphicBoxAdapter >> newPanel [

	^ PanelMorph new 
		changeTableLayout;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		color: Color transparent;
		borderColor: Color transparent;
		yourself
]

{ #category : #factory }
SpMorphicBoxAdapter >> newVertical [

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

{ #category : #accessing }
SpMorphicBoxAdapter >> remove: aPresenter [

	| morph |
	morph := aPresenter adapter widget.	
	widget submorphs first removeMorph: morph.
	widget submorphs second removeMorph: morph.
]

{ #category : #factory }
SpMorphicBoxAdapter >> removeSubWidgets [
	widget submorphs first removeAllMorphs.
	widget submorphs second removeAllMorphs
]
