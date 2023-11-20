Class {
	#name : 'RBBrowseReferencedInstanceVariableChoice',
	#superclass : 'RBVariableChoice',
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'accessing' }
RBBrowseReferencedInstanceVariableChoice >> action [ 

	driver browseInstanceVariableReferences
]

{ #category : 'accessing' }
RBBrowseReferencedInstanceVariableChoice >> description [ 

	^ 'Browse referenced instance variable (s)'
]
