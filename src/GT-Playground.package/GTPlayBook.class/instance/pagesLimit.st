accessing
pagesLimit
	^ pagesLimit ifNil: [ self defaultPagesLimit ]