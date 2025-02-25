"
I am a notice for syntaxic or semantic warnings
"
Class {
	#name : 'OCWarningNotice',
	#superclass : 'OCNotice',
	#category : 'AST-Core-Notice',
	#package : 'AST-Core',
	#tag : 'Notice'
}

{ #category : 'testing' }
OCWarningNotice >> isWarning [

	^ true
]
