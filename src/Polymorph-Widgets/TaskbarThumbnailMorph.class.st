"
I am the helpful little window that appear when you hover a taskbar button. I show a recent screenshot of the window in question and an easy way to close that window.
"
Class {
	#name : 'TaskbarThumbnailMorph',
	#superclass : 'PanelMorph',
	#instVars : [
		'window',
		'taskbarButton',
		'deleteAt',
		'taskbar'
	],
	#category : 'Polymorph-Widgets-Themes',
	#package : 'Polymorph-Widgets',
	#tag : 'Themes'
}

{ #category : 'instance creation' }
TaskbarThumbnailMorph class >> for: aWindow taskbarButton: aButton theme: aTheme [
	^ self new
		window: aWindow;
		button: aButton;
		theme: aTheme;
		initializeAppearance;
		yourself.
]

{ #category : 'fading' }
TaskbarThumbnailMorph >> aboutToBeDeleted [
	deleteAt := DateAndTime now + 150 milliSeconds
]

{ #category : 'accessing' }
TaskbarThumbnailMorph >> button: aButton [
	self assert: taskbarButton isNil description: 'Do not change the taskbar button after creation'.
	taskbarButton := aButton.
	taskbar := aButton owner.

	taskbarButton
		on: #mouseEnter
		send: #isNotBeingDeleted
		to: self;
	
		on: #mouseLeave
		send: #aboutToBeDeleted
		to: self
]

{ #category : 'submorphs - add/remove' }
TaskbarThumbnailMorph >> delete [
	"NOTE: We should only remove it if we are the current thumbnail"
	taskbar removeProperty: #taskbarThumbnail.
	super delete
]

{ #category : 'event handling' }
TaskbarThumbnailMorph >> handlesMouseOver: evt [

	^true
]

{ #category : 'initialization' }
TaskbarThumbnailMorph >> initializeAppearance [
	| thumb closeButton labelAndButton |
	thumb := window taskbarThumbnail.
	closeButton := (self theme
		newCloseControlIn: window
		for: window
		action: [window closeBoxHit. self delete]
		help: 'Close this window' translated)
		extent: window boxExtent.
	labelAndButton := PanelMorph new.
	labelAndButton
		layoutPolicy: RowLayout new;
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap;
		addMorphBack: closeButton;
		addMorphBack: (self theme
			buttonLabelForText: (window labelString truncateWithElipsisTo: 50)).
	self
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap;
		changeTableLayout;
		layoutInset: 8 scaledByDisplayScaleFactor;
		cellInset: 4 scaledByDisplayScaleFactor;
		addMorphBack: thumb;
		addMorphBack: labelAndButton;
		extent: self minExtent;
		fillStyle: (self theme taskListFillStyleFor: self);
		borderStyle: (self theme taskbarThumbnailNormalBorderStyleFor: self);
		cornerStyle: (self theme taskbarThumbnailCornerStyleFor: self).
]

{ #category : 'fading' }
TaskbarThumbnailMorph >> isNotBeingDeleted [ 
	deleteAt := nil
]

{ #category : 'event handling' }
TaskbarThumbnailMorph >> mouseEnter: evt [
	self isNotBeingDeleted
]

{ #category : 'event handling' }
TaskbarThumbnailMorph >> mouseLeave: evt [
	self aboutToBeDeleted
]

{ #category : 'stepping and presenter' }
TaskbarThumbnailMorph >> step [
	deleteAt ifNil: [^self].

	(self containsPoint: self activeHand position) ifTrue: [
		self isNotBeingDeleted.
		^self].

	deleteAt < DateAndTime now ifTrue: [self delete]
]

{ #category : 'stepping and presenter' }
TaskbarThumbnailMorph >> stepTime [
	^ 50
]

{ #category : 'stepping and presenter' }
TaskbarThumbnailMorph >> wantsSteps [
	^ true
]

{ #category : 'accessing' }
TaskbarThumbnailMorph >> window [
	^ window
]

{ #category : 'accessing' }
TaskbarThumbnailMorph >> window: aWindow [ 
	window := aWindow
]
