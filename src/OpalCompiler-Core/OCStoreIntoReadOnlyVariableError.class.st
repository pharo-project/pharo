"
Error when trying to assign to a non writeble Variable.
(method arguments). For backward compatibility this error does not signal if the compilation context is not  interactive.
"
Class {
	#name : #OCStoreIntoReadOnlyVariableError,
	#superclass : #OCSemanticError,
	#category : #'OpalCompiler-Core-Exception'
}

{ #category : #accessing }
OCStoreIntoReadOnlyVariableError >> description [
	"Return a textual description of the exception."

	^ String
		streamContents: [ :stream | 
			stream << super description asString.
			stream cr.
			stream << self methodClass asString.
			stream cr.
			stream << self methodNode asString ]
]

{ #category : #signaling }
OCStoreIntoReadOnlyVariableError >> signal [
	compilationContext interactive
		ifFalse: [ ^ SystemNotification signal: self asString ].
	^ super signal
]
