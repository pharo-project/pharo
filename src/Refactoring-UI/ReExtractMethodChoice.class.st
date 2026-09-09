Class {
	#name : 'ReExtractMethodChoice',
	#superclass : 'ReMethodChoice',
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'accessing' }
ReExtractMethodChoice >> action [

	driver extractMethod
]

{ #category : 'accessing' }
ReExtractMethodChoice >> description [

	^ description ifNil: [ description := 'Extract method' ]
]
