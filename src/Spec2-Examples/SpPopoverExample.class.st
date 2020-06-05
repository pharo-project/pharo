"
I am a simple example of a popover. 
I will show a button pad to allow an user to display a popup in different places.

[[[
self new openWithSpec
]]]
"
Class {
	#name : #SpPopoverExample,
	#superclass : #SpPresenter,
	#instVars : [
		'top',
		'left',
		'bottom',
		'right',
		'popover'
	],
	#category : #'Spec2-Examples-Standalone'
}

{ #category : #initialization }
SpPopoverExample class >> defaultSpec [

	^ SpBoxLayout newHorizontal
		hAlignCenter;
		vAlignCenter; 
		add: #top expand: false;
		add: #left  expand: false;
		add: #bottom expand: false;
		add: #right expand: false;
		yourself
]

{ #category : #initialization }
SpPopoverExample >> initializePresenters [

		top := self newButton
			label: 'Top';
			action: [ self showPopover: SpPopoverPosition top relativeTo: top ].
		left := self newButton
			label: 'Left';
			action: [ self showPopover: SpPopoverPosition left relativeTo: left ].
		bottom := self newButton
			label: 'Bottom';
			action: [ self showPopover: SpPopoverPosition bottom relativeTo: bottom ].
		right := self newButton
			label: 'Right';
			action: [ self showPopover: SpPopoverPosition right relativeTo: right ].
]

{ #category : #initialization }
SpPopoverExample >> initializeWindow: aWindowPresenter [

	aWindowPresenter 
		title: 'Popover example';
		initialExtent: 800@600
]

{ #category : #initialization }
SpPopoverExample >> showPopover: aPosition relativeTo: aPresenter [

	popover ifNotNil: [ popover dismiss ].
	(popover := self newPopover)
		relativeTo: aPresenter;
		position: aPosition;
		presenter: (SpPresenter new 
			layout: (SpBoxLayout newVertical
				borderWidth: 10; 
				spacing: 5;
				add: (self newLabel label: 'A cool popup!');
				add: (self newButton 
					label: 'Dismiss';
					action: [ popover dismiss ];
					yourself);
				yourself);
			yourself);
		popup
]
