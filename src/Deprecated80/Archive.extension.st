Extension { #name : #Archive }

{ #category : #'*Deprecated80' }
Archive >> writeToFileNamed: aFileName [

	self deprecated: 'use ... writeToFile: instead'.
	
	^ self writeToFile: aFileName
]
