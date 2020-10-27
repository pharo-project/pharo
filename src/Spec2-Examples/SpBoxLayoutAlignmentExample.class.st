Class {
	#name : #SpBoxLayoutAlignmentExample,
	#superclass : #SpPresenter,
	#category : #'Spec2-Examples-Layouts'
}

{ #category : #initialization }
SpBoxLayoutAlignmentExample >> initializePresenters [
	<script: 'self new openWithSpec'>
	| pages |
	
	layout := SpBoxLayout newVertical
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

	^ SpBoxLayout newVertical
		add: (SpBoxLayout newHorizontal 
			hAlignStart;
			add: (SpLabelPresenter new label: 'hAlignStart') expand: false;
			yourself);
		add: (SpBoxLayout newHorizontal 
			hAlignCenter;
			add: (SpLabelPresenter new label: 'hAlignCenter') expand: false;
			yourself);
		add: (SpBoxLayout newHorizontal 
			hAlignEnd;
			add: (SpLabelPresenter new label: 'hAlignEnd') expand: false;
			yourself);
		yourself
	
]

{ #category : #initialization }
SpBoxLayoutAlignmentExample >> newVerticalAlignmentLayout [

	^ SpBoxLayout newHorizontal
		add: (SpBoxLayout newVertical 
			vAlignStart;
			add: (SpLabelPresenter new label: 'vAlignStart') expand: false;
			yourself);
		add: (SpBoxLayout newVertical 
			vAlignCenter;
			add: (SpLabelPresenter new label: 'vAlignCenter') expand: false;
			yourself);
		add: (SpBoxLayout newVertical 
			vAlignEnd;
			add: (SpLabelPresenter new label: 'vAlignEnd') expand: false;
			yourself);
		yourself
	
]
