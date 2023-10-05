"
I model ""super""
"
Class {
	#name : 'SuperVariable',
	#superclass : 'PseudoVariable',
	#category : 'Kernel-Variables',
	#package : 'Kernel',
	#tag : 'Variables'
}

{ #category : 'accessing' }
SuperVariable class >> variableName [
	^#super
]

{ #category : 'visiting' }
SuperVariable >> acceptVisitor: aProgramNodeVisitor node: aNode [
	^ aProgramNodeVisitor visitSuperNode: aNode
]

{ #category : 'code generation' }
SuperVariable >> emitValue: methodBuilder [
	"super references the receiver, send that follows is a super send (the message lookup starts in the superclass, see OCASTTranslator>>#emitMessageNode:)"
	methodBuilder pushReceiver
]

{ #category : 'testing' }
SuperVariable >> isSuperVariable [
	^true
]

{ #category : 'debugging' }
SuperVariable >> readInContext: aContext [
	"super in a block is the receiver of the home context
	For clean blocks it might not be known (nil)"
	^aContext home ifNotNil: [:home | home receiver ]
]

{ #category : 'queries' }
SuperVariable >> usingMethods [
	"As super is just a push Self, this detects real super sends, not accesses to super which should never happen"
	^ environment allMethods select: [ :method |
		  method sendsToSuper ]
]
