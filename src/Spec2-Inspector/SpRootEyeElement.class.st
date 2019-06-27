"
RootEyeElement is used in the tree/explorer view for the initial, top root element. It shows a different label.

"
Class {
	#name : #SpRootEyeElement,
	#superclass : #SpSelfEyeElement,
	#category : #'Spec2-Inspector-Element'
}

{ #category : #accessing }
SpRootEyeElement >> label [
	^ 'root'
]
