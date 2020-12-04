Class {
	#name : #SpVerticalPanedLayoutAdapterTest,
	#superclass : #SpPanedLayoutAdapterTest,
	#category : #'Spec2-Backend-Tests-Layout'
}

{ #category : #accessing }
SpVerticalPanedLayoutAdapterTest >> newLayout [
	
	^ SpPanedLayout newVertical
]
