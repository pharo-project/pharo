setup
setUp
	super setUp.
	createdClasses := OrderedCollection new.
	createdPackages := OrderedCollection new.
	packageOrganizer := self packageOrganizerClass basicNew initialize.
	packageOrganizer debuggingName: 'For PackageOrganizerTest'.
	previousAuthor := Author fullName ifNil: [Author fullName: 'RPackage'].
	
	self createNewClassNamed: 'QuadrangleForTesting' inCategory: self class category.
	self quadrangleClass compileSilently: 'intersect:aPoint ^ false'.