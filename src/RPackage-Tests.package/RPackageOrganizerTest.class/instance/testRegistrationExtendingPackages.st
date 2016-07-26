tests extending
testRegistrationExtendingPackages
	"self debug: #testRegistrationExtendingPackages"

	| p |
	self assert: (self packageOrganizer extendingPackagesOf: self quadrangleClass) isEmpty.
	p := self pointRectangleInGraphElement.
	(self packageOrganizer
					registerExtendingPackage:  p
					forClass: self quadrangleClass).
	self deny: (self packageOrganizer extendingPackagesOf: self quadrangleClass) isEmpty.
	self assert: (self packageOrganizer extendingPackagesOf: self quadrangleClass) anyOne name = #GraphElement.
	