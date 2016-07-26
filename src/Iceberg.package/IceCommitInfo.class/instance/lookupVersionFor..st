private
lookupVersionFor: anIceSavedPackage
	"I do not have my self a version for that package, so look for a version in a parent commit."
		| parentVersions |
	self parents size = 0 ifTrue: [ ^ nil ].
	self parents size = 1 ifTrue: [ ^ self parents first versionFor: anIceSavedPackage ].
	
	"Multiple parents still not handled"
	self error: 'I can not handle looking up versions in multiple branches at the same time'
