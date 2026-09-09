Class {
	#name : 'OCUndeclaredVariableNotice',
	#superclass : 'OCErrorNotice',
	#category : 'OpalCompiler-Core-FrontEnd',
	#package : 'OpalCompiler-Core',
	#tag : 'FrontEnd'
}

{ #category : 'testing' }
OCUndeclaredVariableNotice >> isErrorOn: anOpalCompiler [ 
	
	| permitFaulty permitUndeclared |	
	"I'm an error if the compiler does not allow faults.
	Otherwise, I'm a warning"
	
	"I'm an error if the setting is not set"
	permitUndeclared := anOpalCompiler permitUndeclared ifNil: [ false ].
	permitFaulty := anOpalCompiler permitFaulty ifNil: [ false ].
	^ permitUndeclared not and: [ permitFaulty not ]
]

{ #category : 'testing' }
OCUndeclaredVariableNotice >> isUndeclaredNotice [

	^ true
]
