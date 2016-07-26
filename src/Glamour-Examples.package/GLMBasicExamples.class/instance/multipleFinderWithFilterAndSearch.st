searching and filtering
multipleFinderWithFilterAndSearch
	<glmBrowser: 'Search' input: '$a to: $z'>
	"self new multipleFinderWithFilterAndSearch openOn: (1 to: 100)"

	| finder |	
	finder := GLMFinder new.
	finder show: [:a | 
		a list
			beMultiple;
			title: 'Simple';
			dynamicActions: [:list | self actionsFor: list].
		a list
			title: 'With filter';
			beMultiple;
			dynamicActions: [:list | self actionsFor: list];
			filterOn: [:text :each | (Smalltalk evaluate: '[:each | ', text, ']') value: each];
			helpMessage:  'Enter a filtering request (e.g., "each > 10")'.
		a list
			title: 'With search';
			beMultiple;
			dynamicActions: [:list | self actionsFor: list];
			searchOn: [:text :each | (Smalltalk evaluate: '[:each | ', text, ']') value: each];
			helpMessage:  'Enter a search request (e.g., "each > 10")' ].
	^ finder 