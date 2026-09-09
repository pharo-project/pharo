"
I am a notice for syntaxic or semantic errors
"
Class {
	#name : 'OCErrorNotice',
	#superclass : 'OCNotice',
	#category : 'AST-Core-Notice',
	#package : 'AST-Core',
	#tag : 'Notice'
}

{ #category : 'testing' }
OCErrorNotice >> isError [

	^ true
]

{ #category : 'testing' }
OCErrorNotice >> isErrorOn: anOpalCompiler [ 

	| permitFaulty |	
	"I'm an error if the compiler does not allow faults.
	Otherwise, I'm a warning"
	
	"I'm an error if the setting is not set"
	permitFaulty := anOpalCompiler permitFaulty.
	^ permitFaulty ifNil: [ true ]
		ifNotNil: [ permitFaulty not ]
]

{ #category : 'testing' }
OCErrorNotice >> isWarningOn: anOpalCompiler [
	
	^ (self isErrorOn: anOpalCompiler) not
]
