housekeeping
removeEmptyMessageCategories
	"Smalltalk removeEmptyMessageCategories"
	self garbageCollect.
	ClassOrganization allInstancesDo: [ :org | org removeEmptyCategories ].
	SystemOrganization removeEmptyCategories.