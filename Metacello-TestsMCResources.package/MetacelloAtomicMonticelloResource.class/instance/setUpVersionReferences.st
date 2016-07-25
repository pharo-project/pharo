running
setUpVersionReferences
	"self reset"

	versionReferences := OrderedCollection new.
	versionReferences
		add: ((GoferVersionReference name: 'GoferFan-dkh.1') -> #GoferFar);
		add: ((GoferVersionReference name: 'GoferBar-dkh.1') -> #Object);
		add: ((GoferVersionReference name: 'GoferFoo-dkh.4') -> #GoferBeau);
		add: ((GoferVersionReference name: 'GoferFoo-dkh.5') -> #GoferBeau);
		add: ((GoferVersionReference name: 'GoferBeau-dkh.15') -> #GoferBar);
		add: ((GoferVersionReference name: 'GoferFaux-tg.30') -> #Object);
		add: ((GoferVersionReference name: 'GoferFar-dkh.4') -> #GoferFoo);
		add: ((GoferVersionReference name: 'GoferBarDependency-dkh.4') -> #Object);
		add: ((GoferVersionReference name: 'GoferFooDependency-lr.4') -> #Object);
		add: ((GoferVersionReference name: 'GoferBarDependency-lr.30') -> #Object);
		add: ((GoferVersionReference name: 'GoferFooDependency-lr.30') -> #Object);
		add: ((GoferVersionReference name: 'GoferBarDependency-lr.35') -> #Object);
		add: ((GoferVersionReference name: 'GoferFooDependency-lr.35') -> #Object);
		yourself
	