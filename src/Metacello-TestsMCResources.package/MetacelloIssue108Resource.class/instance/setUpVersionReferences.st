running
setUpVersionReferences
	"self reset"

	versionReferences := OrderedCollection new.
	versionReferences
		add: (GoferVersionReference name: 'GoferBar.gemstone-dkh.68');
		add: (GoferVersionReference name: 'GoferBar.gemstone-dkh.69');
		add: (GoferVersionReference name: 'GoferBar.gemstone-dkh.70');
		add: (GoferVersionReference name: 'GoferBar-dkh.68');
		add: (GoferVersionReference name: 'GoferBar-dkh.69');
		add: (GoferVersionReference name: 'GoferBar-dkh.70');
		yourself
	