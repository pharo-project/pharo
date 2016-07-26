tests
testFinderWithPreviousPort
	| browser |
	browser := GLMFinder new.
	browser show: [:a | 
		a list
			title: 'Presentation1';
			display: [ :each | 1 to: each ]].
	browser 
		fromPrevious: #selection;
		passivelyFromPrevious: #highlight;
		andShow: [ :a | 
			a list
				title: 'Presentation2';
				display: [ :selection :highlight | 1 to: selection + highlight ] ].
	browser startOn: 42.
	self assert: browser panes first presentations size equals: 1.
	self assert: browser presentations first title equals: 'Presentation1'.
	self assert: browser transmissions size equals: 1.
	self assert: browser transmissions last origins size equals: 2.
	self assert: browser transmissions last transmissionStrategy presentations size equals: 1.
	self assert: browser transmissions last transmissionStrategy presentations first title equals: 'Presentation2'.
	self assert: (browser panes first port: #entity) value equals: 42.
	(browser panes first port: #highlight) value: 43.
	(browser panes first port: #selection) value: 41.
	self assert: browser panes size equals: 2.
	self assert: browser panes last presentations size equals: 2.
	self assert: browser panes last presentations last displayValue size equals: 84.
	self assert: browser panes last presentations size equals: 2