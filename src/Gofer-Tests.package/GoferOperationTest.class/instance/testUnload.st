tests
testUnload
	gofer
		package: 'GoferFoo';
		load.
	gofer unload.
	self deny: (self hasPackage: 'GoferFoo').
	self deny: (self hasClass: #GoferFoo)