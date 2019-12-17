"
I get signalled when a variable in a block or method scope shadows a variable of the same name in an outer scope.  The default action is to allow it.
"
Class {
	#name : #OCShadowVariableWarning,
	#superclass : #OCSemanticWarning,
	#instVars : [
		'shadowedVar'
	],
	#category : #'OpalCompiler-Core-Exception'
}

{ #category : #correcting }
OCShadowVariableWarning >> defaultAction [ 

	^ self resume: (self variable: self node shadows: self shadowedVar)
	
]

{ #category : #accessing }
OCShadowVariableWarning >> node: aVariableNode [

	super node: aVariableNode.
	messageText := 'Temp shadows: ', aVariableNode name.

]

{ #category : #correcting }
OCShadowVariableWarning >> openMenuIn: aBlock [
	self error: 'should not be called'
]

{ #category : #accessing }
OCShadowVariableWarning >> shadowedVar [

	^ shadowedVar
]

{ #category : #accessing }
OCShadowVariableWarning >> shadowedVar: aVar [

	shadowedVar := aVar
]

{ #category : #correcting }
OCShadowVariableWarning >> showWarningOnTranscript [
	SystemNotification signal: self warningMessage
]

{ #category : #accessing }
OCShadowVariableWarning >> stringMessage [
	^ 'Name already defined'
]

{ #category : #correcting }
OCShadowVariableWarning >> variable: varNode shadows: semVar [
	compilationContext interactive
		ifTrue: [ 
			OCSemanticError new
				node: node;
				compilationContext: compilationContext;
				messageText: self stringMessage;
				signal ]
		ifFalse: [ self showWarningOnTranscript ].

]

{ #category : #accessing }
OCShadowVariableWarning >> warningMessage [

	^ String streamContents: [ :stream |
		stream	 
			nextPutAll: self methodClass name;
			nextPutAll: '>>';
			nextPutAll: self node methodNode selector;
			nextPutAll: '(';
			nextPutAll: self node name;
			nextPutAll: ' is shadowed)'	
	]
]
