initialization
initializeOn: aCollection
	gofer := Gofer new.
	aCollection do: [ :each | gofer package: each ]