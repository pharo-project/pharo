Class {
	#name : #SpBoxLayoutAlignmentExample,
	#superclass : #SpPresenter,
	#category : #'Spec2-Examples-Layouts'
}

{ #category : #initialization }
SpBoxLayoutAlignmentExample >> initializePresenters [
	<script: 'self new openWithSpec'>
	| pages |
	
	layout := SpBoxLayout newTopToBottom
		add: (pages := self newNotebook);
		yourself.
		
	pages 
		addPageTitle: 'Horizontal'
		provider: [ 
			SpPresenter new 
				layout: self newHorizontalAlignmentLayout;
				yourself ].
			
	pages 
		addPageTitle: 'Vertical'
		provider: [ 
			SpPresenter new 
				layout: self newVerticalAlignmentLayout;
				yourself ]
]

{ #category : #initialization }
SpBoxLayoutAlignmentExample >> newHorizontalAlignmentLayout [

	^ SpBoxLayout newTopToBottom
		add: (SpBoxLayout newLeftToRight 
			hAlignStart;
			add: (SpLabelPresenter new label: 'hAlignStart') expand: false;
			yourself);
		add: (SpBoxLayout newLeftToRight 
			hAlignCenter;
			add: (SpLabelPresenter new label: 'hAlignCenter') expand: false;
			yourself);
		add: (SpBoxLayout newLeftToRight 
			hAlignEnd;
			add: (SpLabelPresenter new label: 'hAlignEnd') expand: false;
			yourself);
		yourself
	
]

{ #category : #initialization }
SpBoxLayoutAlignmentExample >> newVerticalAlignmentLayout [

	^ SpBoxLayout newLeftToRight
		add: (SpBoxLayout newTopToBottom 
			vAlignStart;
			add: (SpLabelPresenter new label: 'vAlignStart') expand: false;
			yourself);
		add: (SpBoxLayout newTopToBottom 
			vAlignCenter;
			add: (SpLabelPresenter new label: 'vAlignCenter') expand: false;
			yourself);
		add: (SpBoxLayout newTopToBottom 
			vAlignEnd;
			add: (SpLabelPresenter new label: 'vAlignEnd') expand: false;
			yourself);
		yourself
	
]
