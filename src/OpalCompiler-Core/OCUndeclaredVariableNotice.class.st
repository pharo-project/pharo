Class {
	#name : #OCUndeclaredVariableNotice,
	#superclass : #RBNotice,
	#category : #'OpalCompiler-Core-FrontEnd'
}

{ #category : #signalling }
OCUndeclaredVariableNotice >> signalError [

	OCUndeclaredVariableWarning new
		notice: self;
		signal
]
