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

	theMorph := self addPadding: constraints padding toMorph: childMorph.
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
			layout direction setRigidityOfNonExpandedMorph: theMorph.
			constraints width ifNotNil: [ :w | theMorph width: w ].
			constraints height ifNotNil: [ :h | theMorph height: h ] ].
	
	^ theMorph 
]

{ #category : #private }
SpMorphicBoxAdapter >> addHorizontalPadding: aNumber toMorph: aMorph [
	
	^ self newWrapMorph
		hResizing: aMorph hResizing;
		vResizing: aMorph vResizing;
		width: aMorph width + (aNumber * 2);
		height: aMorph height;
		addMorph: (aMorph
				hResizing: #spaceFill;
				vResizing: #spaceFill;
				yourself)
			fullFrame: (LayoutFrame identity
				leftOffset: aNumber;
				rightOffset: aNumber negated);
		yourself
]

{ #category : #private }
SpMorphicBoxAdapter >> addPadding: aNumber toMorph: aMorph [
	
	aNumber = 0 ifTrue: [ ^ aMorph ].
	
	^ layout direction 
		addPadding: aNumber 
		toMorph: aMorph
		into: self
]

{ #category : #private }
SpMorphicBoxAdapter >> addVerticalPadding: aNumber toMorph: aMorph [
	
	^ self newWrapMorph
		hResizing: aMorph hResizing;
		vResizing: aMorph vResizing;
		width: aMorph width;
		height: aMorph height + (aNumber * 2);
		addMorph: (aMorph
				hResizing: #spaceFill;
				vResizing: #spaceFill;
				yourself)
			fullFrame: (LayoutFrame identity
				topOffset: aNumber;
				bottomOffset: aNumber negated);
		yourself
]

{ #category : #private }
SpMorphicBoxAdapter >> basicAdd: aPresenter constraints: constraints to: aPanel [
	| childMorph |

	childMorph := self buildMorph: aPresenter constraints: constraints.	
	constraints isPlacedAtStart
		ifTrue: [ aPanel submorphs first addMorphBack: childMorph ]
		ifFalse: [ aPanel submorphs second addMorphBack: childMorph ].
	
	aPanel height: (aPanel height max: childMorph height).
	aPanel width: (aPanel width max: childMorph width).
		
	^ childMorph
]

{ #category : #accessing }
SpMorphicBoxAdapter >> childrenWidgets [
	
	^ widget submorphs first submorphs, widget submorphs second submorphs
]

{ #category : #factory }
SpMorphicBoxAdapter >> layout: aLayout [

	layout := aLayout.
	
	widget ifNil: [
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

{ #category : #private }
SpMorphicBoxAdapter >> newWrapMorph [

	^ Morph new
		layoutPolicy: ProportionalLayout new;
		color: Color transparent;
		borderColor: Color transparent
		yourself
]

{ #category : #accessing }
SpMorphicBoxAdapter >> remove: aPresenter [
	| morph |

	morph := aPresenter adapter widget.	
	widget submorphs first removeMorph: morph.
	widget submorphs second removeMorph: morph.
]

{ #category : #accessing }
SpMorphicBoxAdapter >> removeAll [

	widget submorphs first removeAllMorphs.
	widget submorphs second removeAllMorphs
]

{ #category : #factory }
SpMorphicBoxAdapter >> removeSubWidgets [

	widget submorphs first removeAllMorphs.
	widget submorphs second removeAllMorphs
]
