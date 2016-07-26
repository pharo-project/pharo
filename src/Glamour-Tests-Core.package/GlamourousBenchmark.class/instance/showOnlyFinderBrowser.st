utilities
showOnlyFinderBrowser 
	"
	self new showOnlyFinderBrowser openOn: 10
	"
	| finder |
	finder := GLMFinder new.
	finder list
		title: 'title';
		display: [:each | 1 to: each ];
		showOnly: 50.

	^ finder