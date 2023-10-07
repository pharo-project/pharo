Extension { #name : 'PackageTagRenamed' }

{ #category : '*Deprecated12' }
PackageTagRenamed >> newCategoryName [

	self deprecated: 'The system of categories is been removed. Please use the packages and tags instead.'.
	^ self package name = self newName
		  ifTrue: [ self package name ]
		  ifFalse: [ self package name , '-' , self newName ]
]

{ #category : '*Deprecated12' }
PackageTagRenamed >> oldCategoryName [

	self deprecated: 'The system of categories is been removed. Please use the packages and tags instead.'.
	^ self package name = self oldName
		  ifTrue: [ self package name ]
		  ifFalse: [ self package name , '-' , self oldName ]
]
