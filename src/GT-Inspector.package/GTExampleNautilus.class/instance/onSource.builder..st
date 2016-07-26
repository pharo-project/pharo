public
onSource: aClass builder: aBuilder
	aClass gtExamplesProvider = aClass ifTrue: [ ^ self ].
	aClass gtExamplesProvider class = aClass ifTrue: [ ^ self ].
	
	(aBuilder item: #'Browse Example Provider ...')
		order: 90001;
		action: [ aClass gtExamplesProvider browse ].