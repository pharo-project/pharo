Class {
	#name : 'ReDeprecateChoice',
	#superclass : 'ReDeprecateClassChoice',
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'execution' }
ReDeprecateChoice >> action [

	driver deprecateClass 
]

{ #category : 'accessing' }
ReDeprecateChoice >> description [

	^ 'Just add `isDeprecated` method to class side'
]
