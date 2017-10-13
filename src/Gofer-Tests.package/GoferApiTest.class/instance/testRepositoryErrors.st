tests - repositories - options
testRepositoryErrors
	gofer
		url: 'http://pharo-project.org/page-that-will-never-ever-exist';
		repository: self monticelloRepository.
	gofer package: 'GoferFoo'.
	gofer enableRepositoryErrors.
	self should: [ gofer resolved ] raise: GoferRepositoryError.
	gofer disableRepositoryErrors.
	gofer resolved