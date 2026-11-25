Extension { #name : 'UITheme' }

{ #category : '*Morphic-Widgets-Taskbar' }
UITheme >> newBasicTaskbarButtonIn: aTaskbar for: aMorph [
	"Answer a taskbar button morph for the given window."

	| lm lab button labSize |
	labSize := (150 // (aTaskbar tasks size + 1) max: 10) min: 30.

	lab := self buttonLabelForText: (aMorph taskbarLabel truncateWithElipsisTo: labSize).

	aMorph taskbarButtonIsActive ifTrue: [ lab emphasis: 1 ].

	lm := self newRowIn: aTaskbar for: {
			      (self taskbarIcon ifNil: [ ^ nil ]) asMorph.
			      lab }.

	lm cellInset: 2 scaledByDisplayScaleFactor.

	button := self
		          newButtonIn: aTaskbar
		          for: aMorph
		          getState: #taskbarButtonIsActive
		          action: #taskbarButtonClicked
		          arguments: #(  )
		          getEnabled: nil
		          label: lm
		          help: nil.

	button
		useSquareCorners;
		wantsYellowButtonMenu: true;
		getMenuSelector: #taskbarButtonMenu:.


	^ button
]

{ #category : '*Morphic-Widgets-Taskbar' }
UITheme >> newTaskbarButtonIn: aTaskbar for: aWindow [
	"Answer a taskbar button morph for the given window."

	^ (TaskbarItemMorph on: aWindow getState: #isActive action: #taskbarButtonClicked)
		initializeFor: aTaskbar;
		yourself
]

{ #category : '*Morphic-Widgets-Taskbar' }
UITheme >> newTaskbarThumbnailIn: aThemedMorph for: aWindow [
	"Answer a taskbar thumbnail morph for the given window."

	|answer thumb|
	thumb := aWindow taskbarThumbnail.
	answer := PanelMorph new
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap;
		changeTableLayout;
		layoutInset: 8 scaledByDisplayScaleFactor;
		cellInset: 4 scaledByDisplayScaleFactor;
		addMorphBack: thumb;
		addMorphBack: ((self
			buttonLabelForText: (aWindow labelString ))
				color: Color white).
	answer
		extent: answer minExtent;
		fillStyle: (self taskListFillStyleFor: answer);
		borderStyle: (self taskbarThumbnailNormalBorderStyleFor: aWindow);
		cornerStyle: (self taskbarThumbnailCornerStyleFor: answer).
	^answer
]
