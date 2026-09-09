Class {
	#name : 'ReInlineMethodChoice',
	#superclass : 'ReMethodChoice',
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'accessing' }
ReInlineMethodChoice >> action [

	driver inlineMethod
]

{ #category : 'accessing' }
ReInlineMethodChoice >> description [ 

	^ 'Inline the method anyway'
]
