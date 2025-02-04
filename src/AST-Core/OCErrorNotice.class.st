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
