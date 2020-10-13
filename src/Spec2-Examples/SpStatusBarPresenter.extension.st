Extension { #name : #SpStatusBarPresenter }

{ #category : #'*Spec2-Examples' }
SpStatusBarPresenter class >> example [
	| presenter pushButton popButton statusBar count |
	
	(presenter := SpPresenter new)
		layout: (SpBoxLayout newVertical 
			add: (SpBoxLayout newHorizontal 
				add: (pushButton := presenter newButton);
				add: (popButton := presenter newButton);
				yourself);
			addLast: (statusBar := presenter newStatusBar) expand: false;
			yourself).
		
	count := 0.
	pushButton 
		label: 'Push'; 
		action: [ 
			count := count + 1. 
			statusBar pushMessage: ('Message {1}' format: { count }) ].
	popButton 
		label: 'Pop';
		action: [ 
			statusBar popMessage ].
		
	^ presenter openWithSpec
]
