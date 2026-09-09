Class {
	#name : 'ReBrowseSuperSendsSentChoice',
	#superclass : 'ReMethodChoice',
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'accessing' }
ReBrowseSuperSendsSentChoice >> action [

	driver browseSuperSendsSent
]

{ #category : 'accessing' }
ReBrowseSuperSendsSentChoice >> description [

	^ 'Browse methods to be pushed up that send super'
]
