Class {
	#name : 'ReInlineAndRemoveMethodChoice',
	#superclass : 'ReMethodChoice',
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'accessing' }
ReInlineAndRemoveMethodChoice >> action [

	driver inlineAndRemoveMethod 
]

{ #category : 'accessing' }
ReInlineAndRemoveMethodChoice >> description [

	^ 'Inline the method and remove it'
]
