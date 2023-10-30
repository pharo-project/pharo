Extension { #name : 'ClassRenamed' }

{ #category : '*Deprecated12' }
ClassRenamed >> category [
	self deprecated: 'The usage of categories is been removed in favor of using the packages and tags API.'.
	^ self classRenamed category
]
