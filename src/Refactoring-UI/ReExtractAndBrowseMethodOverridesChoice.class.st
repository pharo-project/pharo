Class {
	#name : 'ReExtractAndBrowseMethodOverridesChoice',
	#superclass : 'ReMethodChoice',
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'accessing' }
ReExtractAndBrowseMethodOverridesChoice >> action [

	driver extractMethod.
	driver browseOverrides
]

{ #category : 'accessing' }
ReExtractAndBrowseMethodOverridesChoice >> description [

	^ description ifNil: [ description := 'Extract method and browse existing methods that will then be overriden' ]
]
