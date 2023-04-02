Class {
	#name : #OCUndeclaredVariableNotice,
	#superclass : #RBErrorNotice,
	#category : #'OpalCompiler-Core-FrontEnd'
}

{ #category : #signalling }
OCUndeclaredVariableNotice >> signalError [

	OCUndeclaredVariableWarning new
		notice: self;
		signal
]
