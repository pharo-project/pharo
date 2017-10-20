running
setUp
	super setUp.
	Author fullName ifNil: [Author fullName: 'RPackage'].