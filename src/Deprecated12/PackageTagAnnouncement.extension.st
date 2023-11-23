Extension { #name : 'PackageTagAnnouncement' }

{ #category : '*Deprecated12' }
PackageTagAnnouncement >> categoryName [

	self deprecated: 'The system of categories is been removed. Please use the packages and tags instead.'.
	^ self tag categoryName
]
