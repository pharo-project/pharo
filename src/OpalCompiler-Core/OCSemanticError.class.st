"
Raises a semantic error during semantic analysis
"
Class {
	#name : #OCSemanticError,
	#superclass : #Error,
	#instVars : [
		'compilationContext',
		'node'
	],
	#category : #'OpalCompiler-Core-Exception'
}

{ #category : #accessing }
OCSemanticError >> compilationContext [
	^ compilationContext
]

{ #category : #accessing }
OCSemanticError >> compilationContext: anObject [
	compilationContext := anObject
]

{ #category : #exceptiondescription }
OCSemanticError >> defaultAction [
	^self notify: messageText at: node start
]

{ #category : #accessing }
OCSemanticError >> methodClass [
	^self methodNode methodClass
]

{ #category : #accessing }
OCSemanticError >> methodNode [
	^node methodNode
]

{ #category : #accessing }
OCSemanticError >> node [
	^ node
]

{ #category : #accessing }
OCSemanticError >> node: aNode [
	node := aNode
]

{ #category : #'error handling' }
OCSemanticError >> notify: aString at: location [
	^self requestor 
		ifNil: [SyntaxErrorNotification
					inClass: self methodClass 
					withCode: self methodNode source
					doitFlag: false
					errorMessage: aString
					location: location]
		ifNotNil: [self requestor
					notify: aString , ' ->'
					at: location
					in: self requestor text]
]

{ #category : #accessing }
OCSemanticError >> requestor [
	^ compilationContext requestor
]
