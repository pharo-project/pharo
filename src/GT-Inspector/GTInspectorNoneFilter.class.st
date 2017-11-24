"
I do not filter presentation and allow all of them to be displayed
"
Class {
	#name : #GTInspectorNoneFilter,
	#superclass : #GTInspectorPresentationFilter,
	#category : #'GT-Inspector'
}

{ #category : #accessing }
GTInspectorNoneFilter >> changeFilterRequestFor: aGTInspector [
	"do nothing"
]

{ #category : #testing }
GTInspectorNoneFilter >> shouldDisplayPresentationCreatedBy: aMethod [
	"display all of them"
	^ true
]
