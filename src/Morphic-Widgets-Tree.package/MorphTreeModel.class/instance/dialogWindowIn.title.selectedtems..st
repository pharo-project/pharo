dialog
dialogWindowIn: aWindow title: aTitle selectedtems: aCollection 
	| dialog dialogMorph |
	dialog := self theme
				newPluggableDialogWindowIn: aWindow
				title: aTitle
				for: (dialogMorph := self defaultMorphIn: aWindow).
	dialogMorph buildContents; selectedItems: aCollection.
	dialog minimumExtent: 300 @ 500.
	dialog beResizeable.
	^ dialog 