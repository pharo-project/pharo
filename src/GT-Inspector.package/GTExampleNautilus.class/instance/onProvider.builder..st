public
onProvider: aClass builder: aBuilder
	aClass gtExamplesProvider = aClass ifTrue: [ ^ self ].
	aClass gtExamplesSource = aClass ifTrue: [ ^ self ].
	
	(aBuilder item: #'Browse Example Source ...')
		order: 90002;
		action: [ aClass gtExamplesSource browse ].