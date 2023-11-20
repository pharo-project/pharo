Class {
	#name : 'RBBrowseReferencedSharedVariableChoice',
	#superclass : 'RBVariableChoice',
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'accessing' }
RBBrowseReferencedSharedVariableChoice >> action [ 

	driver browseSharedVariableReferences
]

{ #category : 'accessing' }
RBBrowseReferencedSharedVariableChoice >> description [ 

	^ 'Browse referenced shared variable (s)'
]
