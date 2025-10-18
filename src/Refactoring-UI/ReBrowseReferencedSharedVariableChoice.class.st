Class {
	#name : 'ReBrowseReferencedSharedVariableChoice',
	#superclass : 'ReVariableChoice',
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'accessing' }
ReBrowseReferencedSharedVariableChoice >> action [ 

	driver browseSharedVariableReferences
]

{ #category : 'accessing' }
ReBrowseReferencedSharedVariableChoice >> description [ 

	^ 'Browse referenced shared variable (s)'
]
