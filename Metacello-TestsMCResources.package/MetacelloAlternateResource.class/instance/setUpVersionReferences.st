running
setUpVersionReferences
	"self reset"

	versionReferences := OrderedCollection new.
	versionReferences
		add: (GoferVersionReference name: 'GoferBar-dkh.24');
		add: (GoferVersionReference name: 'GoferBar-dkh.25');
		add: (GoferVersionReference name: 'GoferFoo-dkh.75');
		add: (GoferVersionReference name: 'GoferFoo-dkh.104')