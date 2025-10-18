Class {
	#name : 'ReBrowseReferencedInstanceVariableChoice',
	#superclass : 'ReVariableChoice',
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'accessing' }
ReBrowseReferencedInstanceVariableChoice >> action [ 

	driver browseInstanceVariableReferences
]

{ #category : 'accessing' }
ReBrowseReferencedInstanceVariableChoice >> description [ 

	^ 'Browse referenced instance variable (s)'
]
