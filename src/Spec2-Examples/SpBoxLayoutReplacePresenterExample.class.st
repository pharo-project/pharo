Class {
	#name : #SpBoxLayoutReplacePresenterExample,
	#superclass : #SpPresenter,
	#category : #'Spec2-Examples-Layouts'
}

{ #category : #initialization }
SpBoxLayoutReplacePresenterExample >> initializePresenters [
	| p1 p2 p3 button |
	<script: 'self new openWithSpec'>
	
	layout := SpBoxLayout newVertical
		add: (p1 := SpLabelPresenter new label: 'Presenter 1');
		add: (p2 := SpLabelPresenter new label: 'Presenter 2'); 
		add: (p3 := SpLabelPresenter new label: 'Presenter 3'); 
		add: (button := SpButtonPresenter new label: 'Replace Presenter 2!') expand: false;
		yourself.
		
	button action: [ 
		layout 
			replace: layout children second 
			with: (SpLabelPresenter new label: 'Replaced!') ]
]
