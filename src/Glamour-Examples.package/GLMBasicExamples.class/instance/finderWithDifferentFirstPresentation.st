browsers
finderWithDifferentFirstPresentation
	<glmBrowser: 'Finder with different first presentation' input: '1 to: 42'>
	"self new finderWithDifferentFirstPresentation openOn: (1 to: 42)"
	| finder |	
	finder := GLMFinder new.
	finder with: [:f |
		f
			showFirst: [:a | a list]; 
			show: [:a | a list display: [:x | 1 to: x]
			]].
	^ finder