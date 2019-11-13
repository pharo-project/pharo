"
I am a warning about the definition of a new Undeclared variable. I may be quite common during loading of packages that do not have (or cannot have) clean dependencies structure or are loaded in wrong order.
"
Class {
	#name : #NewUndeclaredWarning,
	#superclass : #SystemNotification,
	#instVars : [
		'undeclaredName',
		'originName'
	],
	#category : #'Kernel-Exceptions'
}

{ #category : #exceptioninstantiator }
NewUndeclaredWarning class >> signal: undeclaredName in: originName [

	^ self new 
		undeclaredName: undeclaredName;
		originName: originName;
		signal.
]

{ #category : #accessing }
NewUndeclaredWarning >> messageText [
	^String streamContents: [ :str |
		originName putOn: str.		
		str nextPutAll: ' ('.
		undeclaredName putOn: str.
		str nextPutAll: ' is Undeclared)' ].
]

{ #category : #accessing }
NewUndeclaredWarning >> originName [
	^ originName
]

{ #category : #accessing }
NewUndeclaredWarning >> originName: anObject [
	originName := anObject
]

{ #category : #accessing }
NewUndeclaredWarning >> undeclaredName [
	^ undeclaredName
]

{ #category : #accessing }
NewUndeclaredWarning >> undeclaredName: anObject [
	undeclaredName := anObject
]
