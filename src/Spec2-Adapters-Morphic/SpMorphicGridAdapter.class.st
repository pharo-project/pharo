"
I'm an adapter for Grid Layouts.
A grid layout will arrange components according to its coordinates and span information.
"
Class {
	#name : #SpMorphicGridAdapter,
	#superclass : #SpMorphicLayoutAdapter,
	#category : #'Spec2-Adapters-Morphic-Layout'
}

{ #category : #private }
SpMorphicGridAdapter >> addConstraints: constraints toChild: childMorph [
	"Adds constraits by child"
	
	childMorph hResizing: #spaceFill.
	childMorph vResizing: #spaceFill.
	childMorph 
		setProperty: #gridConstraints 
		toValue: constraints.
	
	^ childMorph
]

{ #category : #private }
SpMorphicGridAdapter >> applyChildrenAlignmentTo: aWidget constraints: constraints [

	constraints hasAlignment 
		ifTrue: [ ^ self basicApplyChildrenAlignmentTo: aWidget constraints: constraints ].
	(layout columnConstraintsAt: constraints column) ifNotNil: [ :axisConstraints |
		axisConstraints hasAlignment 
			ifTrue: [ ^ self basicApplyChildrenAlignmentTo: aWidget constraints: axisConstraints ] ].
	(layout rowConstraintsAt: constraints row) ifNotNil: [ :axisConstraints |
		axisConstraints hasAlignment 
			ifTrue: [ ^ self basicApplyChildrenAlignmentTo: aWidget constraints: axisConstraints ] ].

	^ aWidget
]

{ #category : #private }
SpMorphicGridAdapter >> basicAdd: aPresenter constraints: constraints to: aPanel [

	| childMorph |
	childMorph := self 
		buildMorph: aPresenter 
		constraints: constraints.
	aPanel addMorphBack: childMorph.
		
	^ childMorph	
]

{ #category : #private }
SpMorphicGridAdapter >> basicApplyChildrenAlignmentTo: aWidget constraints: constraints [

	| alignmentWidget |	
	constraints hasAlignment ifFalse: [ ^ aWidget ].
	
	aWidget 
		changeTableLayout;
		hResizing: #shrinkWrap; 
		vResizing: #shrinkWrap.
	
	alignmentWidget := Morph new
		changeTableLayout; 
		color: Color transparent;
		addMorphBack: aWidget.

	constraints vAlign ifNotNil: [ :align |
		alignmentWidget listCentering: align asMorphicAlign ].
	constraints hAlign ifNotNil: [ :align |
		alignmentWidget wrapCentering: align asMorphicAlign ].
		
	^ alignmentWidget
]

{ #category : #accessing }
SpMorphicGridAdapter >> children [
	
	^ widget submorphs
]

{ #category : #factory }
SpMorphicGridAdapter >> layout: aLayout [
	"Build of widget was differed up to here (to know what is the direction)"

	layout := aLayout.

	widget ifNil: [ 
		widget := self newPanel.
		widget layoutPolicy layout: aLayout ].
	
	aLayout whenChildrenAddedDo: [ :ann |
		self
			add: ann child
			constraints: (aLayout constraintsFor: ann child) ].
	
	aLayout whenChildrenRemovedDo: [ :ann |
		self remove: ann child ].

	super layout: aLayout
]

{ #category : #factory }
SpMorphicGridAdapter >> newPanel [

	^ super newPanel
		layoutPolicy: SpMorphicGridLayout new;
		yourself
]

{ #category : #removing }
SpMorphicGridAdapter >> remove: aPresenter [

	| morph |
	morph := aPresenter adapter widget.
	widget removeMorph: morph
]
