"
I'm a morphic layout that superposes morphs.
I have a main submorph and an overlay that is drawn on top.
To decide how to lay out each of these submorphs, they need a property named #constraints with a constraint object.
If the constraints object indicates it is an overlay object, it will be rendered on top.
Otherwise, it will be rendered first in the bottom.

Example

p := PanelMorph new
	hResizing: #spaceFill;
	vResizing: #spaceFill;
	layoutPolicy: SpMorphicOverlayLayout new.

mainMorph := Morph new
	hResizing: #spaceFill;
	vResizing: #spaceFill;
	setProperty: #constraints
	toValue: SpOverlayConstraints new.

overlay := Morph new
	hResizing: #spaceFill;
	vResizing: #spaceFill;
	color: Color green;
	setProperty: #constraints
	toValue: (SpOverlayConstraints new beOverlay; yourself);
	yourself.

p
	addMorphBack: overlay;
	addMorphBack: mainMorph.
p openInWindow
"
Class {
	#name : #SpMorphicOverlayLayout,
	#superclass : #LayoutPolicy,
	#category : #'Spec2-Adapters-Morphic-Layout'
}

{ #category : #layout }
SpMorphicOverlayLayout >> layout: aMorph in: layoutBounds [
	aMorph submorphsDo: [ :m | | constraints |
		constraints := m valueOfProperty: #constraints.
		(m visible and: [ constraints isOverlay not ])
			ifTrue: [ m bounds: layoutBounds ].
		(m visible and: [ constraints isOverlay ])
			ifTrue: [ | morphBounds left top newBounds |
				morphBounds := layoutBounds area > (0@0 corner: m extent) area
					ifTrue: [ layoutBounds origin corner: (layoutBounds origin + m extent) ]
					ifFalse: [ layoutBounds ].
				
				left := (layoutBounds width / 2) - (morphBounds width / 2).
				top := (layoutBounds height / 2) - (morphBounds height / 2).
				newBounds := (layoutBounds origin + (left@top) corner: layoutBounds corner + (left@top)).

				m bounds: newBounds ].
		m visible
			ifFalse: [ m bounds: (0@0 corner: 0@0) ] ]
]
