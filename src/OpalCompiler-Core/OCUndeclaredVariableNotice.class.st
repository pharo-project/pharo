Class {
	#name : 'OCUndeclaredVariableNotice',
	#superclass : 'ASTErrorNotice',
	#category : 'OpalCompiler-Core-FrontEnd',
	#package : 'OpalCompiler-Core',
	#tag : 'FrontEnd'
}

{ #category : 'testing' }
OCUndeclaredVariableNotice >> isUndeclaredNotice [

	^ true
]

{ #category : 'correcting' }
OCUndeclaredVariableNotice >> reparator [

	^ OCCodeReparator new node: self node
]
