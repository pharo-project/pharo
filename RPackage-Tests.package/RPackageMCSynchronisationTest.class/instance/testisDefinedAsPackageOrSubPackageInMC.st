tests
testisDefinedAsPackageOrSubPackageInMC
	"test whether a given category is defined as a MC package or whether it is defined as top part of package"
	
	"self debug: #testisDefinedAsSubPackageInMC"
	"self debug: #testisDefinedAsPackageOrSubPackageInMC"
	
	self assert: (self organizer isDefinedAsPackageOrSubPackageInMC: #'Compression').
	self deny: (self organizer isDefinedAsPackageOrSubPackageInMC: #'Compression-Archives').
	"Indeed compression-archives is a category but not a MC package"
	
	self assert: (self organizer isDefinedAsPackageOrSubPackageInMC: #'Collections').
	"#Collection is not defined as a package but as a package #'Collections-Unordered'"
	self assert: (self organizer isDefinedAsPackageOrSubPackageInMC: #'Collections-Unordered').
	self deny: (self organizer isDefinedAsPackageOrSubPackageInMC: #'Collections-Unordered-Test').