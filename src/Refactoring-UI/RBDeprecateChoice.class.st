Class {
	#name : 'RBDeprecateChoice',
	#superclass : 'RBDeprecateClassChoice',
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'execution' }
RBDeprecateChoice >> action [

	driver deprecateClass 
]

{ #category : 'accessing' }
RBDeprecateChoice >> description [

	^ 'Just add `isDeprecated` method to class side'
]
