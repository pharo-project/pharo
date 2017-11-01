Extension { #name : #MCHttpRepository }

{ #category : #'*Komitter-Models' }
MCHttpRepository >> isPrivatePharoRepository [
	
	^ self class pharoLocations includes: self location
]

{ #category : #'*Komitter-Models' }
MCHttpRepository >> isRemote [
	^ self isPrivatePharoRepository not
]

{ #category : #'*Komitter-Models' }
MCHttpRepository >> koRemote [

	^ KomitHttpRemote new
		remote: self;
		yourself
]

{ #category : #'*Komitter-Models' }
MCHttpRepository class >> pharoLocations [
	"Answer the locations for both pharo and pharo inbox repositories.
	No commit should actually be pushed directly there"

	^ #('http://smalltalkhub.com/mc/Pharo/Pharo60/main' 
		'http://smalltalkhub.com/mc/Pharo/Pharo60/main/' 
		'http://smalltalkhub.com/mc/Pharo/Pharo60Inbox/main'
		'http://smalltalkhub.com/mc/Pharo/Pharo60Inbox/main/'
		'http://smalltalkhub.com/mc/Pharo/Pharo50Inbox/main'
		'http://smalltalkhub.com/mc/Pharo/Pharo50Inbox/main/')
]
