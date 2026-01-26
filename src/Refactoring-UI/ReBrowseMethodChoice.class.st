Class {
	#name : 'ReBrowseMethodChoice',
	#superclass : 'ReMethodChoice',
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'actions' }
ReBrowseMethodChoice >> action [
	
	driver browseMethod
]

{ #category : 'accessing' }
ReBrowseMethodChoice >> description [ 

	^ 'Browse the method'
]
