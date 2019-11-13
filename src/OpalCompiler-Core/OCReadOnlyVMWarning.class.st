"
I get signalled when the compiler attempts to set a literal as read-only while the VM does not support read-only objects.
"
Class {
	#name : #OCReadOnlyVMWarning,
	#superclass : #OCSemanticWarning,
	#category : #'OpalCompiler-Core-Exception'
}

{ #category : #initialization }
OCReadOnlyVMWarning >> defaultAction [
	^ self
		resume:
			(compilationContext interactive
				ifTrue: [ OCSemanticError new
						node: node;
						compilationContext: compilationContext;
						messageText: self messageText;
						signal ]
				ifFalse: [ SystemNotification signal: self messageText ])
]

{ #category : #initialization }
OCReadOnlyVMWarning >> initialize [

	super initialize.
	messageText := 'Bytecode compiler is trying to set a literal as read-only but the VM used does not support read-only objects'.
]

{ #category : #correcting }
OCReadOnlyVMWarning >> openMenuIn: aBlock [
	self error: 'should not be called'
]
