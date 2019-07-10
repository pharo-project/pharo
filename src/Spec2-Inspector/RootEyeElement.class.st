"
RootEyeElement is used in the tree/explorer view for the initial, top root element. It shows a different label.

"
Class {
	#name : #RootEyeElement,
	#superclass : #SelfEyeElement,
	#category : #'Spec2-Inspector-Element'
}

{ #category : #accessing }
RootEyeElement >> label [
	^ 'root'
]
