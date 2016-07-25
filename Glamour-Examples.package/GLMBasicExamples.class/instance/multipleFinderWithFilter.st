searching and filtering
multipleFinderWithFilter
	<glmBrowser: 'Filter' input: '$a to: $z'>
	"self new multipleFinderWithFilter openOn: ($a to: $z)"

	| finder |	
	finder := GLMFinder new.
	finder show: [:a | 
		a list
			title: 'List';
			beMultiple;
			dynamicActions: [:list | self actionsFor: list];
			filterOn: [:text :each | Smalltalk compiler evaluate: '| each | each := self. ', text for: each logged: false ];
			helpMessage:  'Enter a filtering request (e.g., "each > $f")'. 
		a tree
			title: 'Tree';
			dynamicActions: [:list | self actionsFor: list];
			filterOn: [:text :each | Smalltalk compiler evaluate: '| each | each := self. ', text for: each logged: false ];
			helpMessage:  'Enter a filtering request (e.g., "each > $f")' ].
	^ finder 