running
setUpVersionReferences
	"This method answers a set of Gofer references in the order they should be sorted. It includes two different packages (Gofer-Foo, Gofer-Bar), linear series of packages (Gofer-Foo-lr.1, Gofer-Foo-lr.2, Gofer-Foo-lr.4), packages with a branch (Gofer-Bar.branch-lr.1,Gofer.branch-Bar-lr.2), and packages with the same version but different authors (Gofer-Bar-jf.1, Gofer-Bar-lr.1)."

	versionReferences := OrderedCollection new.
	versionReferences
		add: (GoferVersionReference name: 'GoferBar.branch-lr.1');
		add: (GoferVersionReference name: 'GoferBar.branch-lr.2');
		add: (GoferVersionReference name: 'GoferBar-jf.1');
		add: (GoferVersionReference name: 'GoferBar-lr.1');
		add: (GoferVersionReference name: 'GoferFoo-lr.1');
		add: (GoferVersionReference name: 'GoferFoo-lr.2');
		add: (GoferVersionReference name: 'GoferFoo-lr.4')