tests
testActualClassTags

	| packageWithoutClassTags packageWithClassTags |
	packageWithoutClassTags := RPackageOrganizer default packageOf: Announcement.
	self deny: packageWithoutClassTags classTags isEmpty.
	self assert: packageWithoutClassTags actualClassTags isEmpty.
	
	packageWithClassTags := RPackageOrganizer default packageOf: Object.
	self assert: packageWithClassTags actualClassTags equals: packageWithClassTags classTags