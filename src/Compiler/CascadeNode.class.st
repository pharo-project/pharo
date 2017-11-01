"
The first message has the common receiver, the rest have receiver == nil, which signifies cascading.
"
Class {
	#name : #CascadeNode,
	#superclass : #ParseNode,
	#instVars : [
		'receiver',
		'messages'
	],
	#category : 'Compiler-ParseNodes'
}

{ #category : #visiting }
CascadeNode >> accept: aVisitor [
	^aVisitor visitCascadeNode: self
]

{ #category : #'code generation (closures)' }
CascadeNode >> analyseTempsWithin: scopeBlock "<BlockNode>" rootNode: rootNode "<MethodNode>" assignmentPools: assignmentPools [ "<Dictionary>"
	{ receiver }, messages do:
		[:node| node analyseTempsWithin: scopeBlock rootNode: rootNode assignmentPools: assignmentPools]
]

{ #category : #'code generation' }
CascadeNode >> emitCodeForValue: stack encoder: encoder [
	receiver emitCodeForValue: stack encoder: encoder.
	1 to: messages size - 1 do: 
		[:i | 
		encoder genDup.
		stack push: 1.
		(messages at: i) emitCodeForValue: stack encoder: encoder.
		encoder genPop.
		stack pop: 1].
	messages last emitCodeForValue: stack encoder: encoder
]

{ #category : #accessing }
CascadeNode >> messages [
	^messages
]

{ #category : #printing }
CascadeNode >> printOn: aStream indent: level [
	self printOn: aStream indent: level precedence: 0
]

{ #category : #printing }
CascadeNode >> printOn: aStream indent: level precedence: p [ 

	p > 0 ifTrue: [aStream nextPut: $(].
	messages first printReceiver: receiver on: aStream indent: level.
	1 to: messages size do: 
		[:i | (messages at: i) printOn: aStream indent: level.
		i < messages size ifTrue: 
				[aStream nextPut: $;.
				messages first precedence >= 2 ifTrue: [aStream crtab: level + 1]]].
	p > 0 ifTrue: [aStream nextPut: $)]
]

{ #category : #accessing }
CascadeNode >> receiver [
	^receiver
]

{ #category : #initialize-release }
CascadeNode >> receiver: receivingObject messages: msgs [
	" Transcript show: 'abc'; cr; show: 'def' "

	receiver := receivingObject.
	messages := msgs
]

{ #category : #'code generation' }
CascadeNode >> sizeCodeForValue: encoder [
	| size |
	size := (receiver sizeCodeForValue: encoder)
			 + (messages size - 1 * (encoder sizeDup + encoder sizePop)).
	messages do: [:aMessage | size := size + (aMessage sizeCodeForValue: encoder)].
	^size
]
