utilities
defaultFinderBrowser 
	"
	self new defaultFinderBrowser openOn: 10
	"
	| finder |
	finder := GLMFinder new.
	finder show: [:a | a list
		title: 'title';
		display: [:each | 1 to: each ];
		showOnly: 50 ].

	^ finder