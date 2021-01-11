Class {
	#name : #SindarinCommand,
	#superclass : #CmCommand,
	#category : #'NewTools-Sindarin-Commands'
}

{ #category : #initialization }
SindarinCommand class >> defaultIconName [
	^self subclassResponsibility 
]

{ #category : #'instance creation' }
SindarinCommand class >> forSpecContext: anObject [
	self flag: 'Why is this class needed besides this helper?'.
	^ (self forSpecWithIconNamed: self defaultIconName)
		context: anObject;
		yourself
]

{ #category : #accessing }
SindarinCommand >> debuggerPresenter [
	^self context
]
