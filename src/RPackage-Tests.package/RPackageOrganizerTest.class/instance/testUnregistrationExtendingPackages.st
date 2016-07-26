tests extending
testUnregistrationExtendingPackages
	"self debug: #testUnregistrationExtendingPackages"

	| p |
	
	
	p := self pointRectangleInGraphElement.
	self packageOrganizer
		registerExtendingPackage: p
		forClass: self quadrangleClass.
	self deny: (self packageOrganizer extendingPackagesOf: self quadrangleClass) isEmpty.
	self assert: (self packageOrganizer extendingPackagesOf: self quadrangleClass) anyOne name = #GraphElement.
	self packageOrganizer
					unregisterExtendingPackage:  p
					forClass: self quadrangleClass.
	self assert: (self packageOrganizer extendingPackagesOf: self quadrangleClass) isEmpty.
	