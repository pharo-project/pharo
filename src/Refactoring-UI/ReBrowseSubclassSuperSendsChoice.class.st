Class {
	#name : 'ReBrowseSubclassSuperSendsChoice',
	#superclass : 'ReMethodChoice',
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'accessing' }
ReBrowseSubclassSuperSendsChoice >> action [

	driver browseSubclassesSuperSends
]

{ #category : 'accessing' }
ReBrowseSubclassSuperSendsChoice >> description [

	^ 'Browse methods from subclasses that send super to the method to be pushed down'
]
