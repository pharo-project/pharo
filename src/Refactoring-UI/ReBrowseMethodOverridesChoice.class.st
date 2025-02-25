Class {
	#name : 'ReBrowseMethodOverridesChoice',
	#superclass : 'ReMethodChoice',
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'accessing' }
ReBrowseMethodOverridesChoice >> action [

	driver browseOverrides
]

{ #category : 'accessing' }
ReBrowseMethodOverridesChoice >> description [

	^ 'Browse existing methods that will be overriden BUT don''t rename'
]
