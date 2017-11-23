"
This property represents exceptions that occured during other properties computation. This way tools can work witout interuptions, but users will be aware the there were some exceptions.
"
Class {
	#name : #ReExceptionProperty,
	#superclass : #ReProperty,
	#instVars : [
		'stack',
		'message'
	],
	#category : #'Renraku-Critiques'
}

{ #category : #'instance creation' }
ReExceptionProperty class >> for: anEntity with: anException [
	
	^ self new
		initializeSourceAnchor: (
			ReSourceAnchor entity: anEntity);
		stack: anException signalerContext copyStack;
		message: anException description;
		yourself
		
]

{ #category : #actions }
ReExceptionProperty >> actions [

	^ {RePropertyAction new
			icon: (self iconNamed: #smallDebug);
			description: 'Debug the exception';
			action: [ :prop |
				UIManager default
					debugProcess: Processor activeProcess 
					context: prop stack
					label: prop message 
					fullView: true ];
			yourself}
]

{ #category : #accessing }
ReExceptionProperty >> icon [
	^ self iconNamed: #exceptionIcon
]

{ #category : #accessing }
ReExceptionProperty >> message [
	^ message
]

{ #category : #accessing }
ReExceptionProperty >> message: anObject [
	message := anObject
]

{ #category : #accessing }
ReExceptionProperty >> stack [
	^ stack
]

{ #category : #accessing }
ReExceptionProperty >> stack: anObject [
	stack := anObject
]

{ #category : #accessing }
ReExceptionProperty >> title [
	^ '~Exception: [', message, ']'
]
