Extension { #name : #Archive }

{ #category : #'*Deprecated80' }
Archive >> canWriteToFileNamed: aFileReferenceOrFileName [
	
	self deprecated: 'use ... canWriteToFile: instead'.
	
	^ self canWriteToFile: aFileReferenceOrFileName
]

{ #category : #'*Deprecated80' }
Archive >> writeToFileNamed: aFileName [

	self deprecated: 'use ... writeToFile: instead'.
	
	^ self writeToFile: aFileName
]
