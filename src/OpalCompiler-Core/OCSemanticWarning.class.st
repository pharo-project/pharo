"
If a variable is not found or some other problem occurs during checking (ASTChecker) than a particular subclass instance of me is signal, usually causing a notification to the user. If not handled, the default handling is done, and compiling continues.
"
Class {
	#name : #OCSemanticWarning,
	#superclass : #Notification,
	#instVars : [
		'compilationContext',
		'node'
	],
	#category : #'OpalCompiler-Core-Exception'
}

{ #category : #accessing }
OCSemanticWarning >> compilationContext [
	^ compilationContext
]

{ #category : #accessing }
OCSemanticWarning >> compilationContext: anObject [
	compilationContext := anObject
]

{ #category : #'accessing - compatibility' }
OCSemanticWarning >> errorCode [
	self requestor ifNil: [
		^ self node methodNode asString ].
	^ self requestor contents
]

{ #category : #'accessing - compatibility' }
OCSemanticWarning >> errorMessage [
	^ self messageText
]

{ #category : #'accessing - compatibility' }
OCSemanticWarning >> location [
	^ self node sourceInterval first
]

{ #category : #correcting }
OCSemanticWarning >> methodClass [
	^node methodNode methodClass
]

{ #category : #correcting }
OCSemanticWarning >> methodNode [
	^node methodNode
]

{ #category : #accessing }
OCSemanticWarning >> node [
	^node
]

{ #category : #accessing }
OCSemanticWarning >> node: anObject [
	node := anObject
]

{ #category : #accessing }
OCSemanticWarning >> requestor [
	^ compilationContext requestor
]
