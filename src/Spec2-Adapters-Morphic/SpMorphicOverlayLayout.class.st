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
