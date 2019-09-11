"
Adapt the presenter on a stub view that displays no real UI elements.
"
Class {
	#name : #SpStubListAdapter,
	#superclass : #SpStubAbstractAdapter,
	#category : #'Spec2-Adapters-Stub-Adapters'
}

{ #category : #factory }
SpStubListAdapter >> buildWidget [

	^ SpStubListView new
]

{ #category : #factory }
SpStubListAdapter >> refreshWidgetList [

]

{ #category : #factory }
SpStubListAdapter >> refreshWidgetSelection [

]
