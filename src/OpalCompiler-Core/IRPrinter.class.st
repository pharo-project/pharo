"
I interpret IRMethod instructions and write them out to a print stream.
"
Class {
	#name : #IRPrinter,
	#superclass : #IRVisitor,
	#instVars : [
		'stream'
	],
	#category : #'OpalCompiler-Core-IR-Manipulation'
}

{ #category : #private }
IRPrinter >> label: seqNum [

	"add tab and cr since this does not get called within interpretInstruction:"
	stream cr.  "extra cr just to space out sequences"
	stream nextPutAll: 'label: '.
	seqNum printOn: stream.
	stream cr.

]

{ #category : #private }
IRPrinter >> stream: stringWriteStream [

	stream := stringWriteStream
]

{ #category : #visiting }
IRPrinter >> visitBlockReturnTop: blockReturnTop [

	stream nextPutAll: 'blockReturnTop'.


]

{ #category : #visiting }
IRPrinter >> visitInstruction: aNode [
	self visitNode: aNode.
	stream cr.
]

{ #category : #visiting }
IRPrinter >> visitJump: jump [

	stream nextPutAll: 'goto: '.
	jump destination orderNumber printOn: stream.

]

{ #category : #visiting }
IRPrinter >> visitJumpIf: jumpIf [

	stream nextPutAll: 'if: '.
	jumpIf boolean printOn: stream.
	stream nextPutAll: ' goto: '.
	jumpIf destination orderNumber printOn: stream.
	stream nextPutAll: ' else: '.
	jumpIf otherwise orderNumber printOn: stream.

]

{ #category : #visiting }
IRPrinter >> visitPop: pop [

	stream nextPutAll: 'popTop'

]

{ #category : #visiting }
IRPrinter >> visitPopIntoInstVar: instVar [
	
	stream nextPutAll: 'popIntoInstVar: '.
	instVar index printOn: stream.

]

{ #category : #visiting }
IRPrinter >> visitPopIntoLiteralVariable: litVar [
	
	| object |
	object := litVar association.
	stream nextPutAll: 'popIntoLiteralVariable: '.
	object isVariableBinding ifTrue: [^ stream nextPutAll: object key].
	object printOn: stream.
]

{ #category : #visiting }
IRPrinter >> visitPopIntoRemoteTemp: remoteTemp [

	stream nextPutAll: 'popIntoRemoteTemp: '.
	remoteTemp name printOn: stream.
	stream nextPutAll: ' inVector: '.
	remoteTemp tempVectorName printOn: stream
]

{ #category : #visiting }
IRPrinter >> visitPopIntoTemp: tmp [

	stream nextPutAll: 'popIntoTemp: '.
	tmp name printOn: stream.



]

{ #category : #visiting }
IRPrinter >> visitPushArray: array [
	
	array cons 
		ifTrue: [  
			stream nextPutAll: 'pushConsArray: ' ]
		ifFalse: [
			stream nextPutAll: 'pushNewArray: ' ].
	array size printOn: stream. 

]

{ #category : #visiting }
IRPrinter >> visitPushClosureCopy: closure [

	stream nextPutAll: 'pushClosureCopyCopiedValues: '.
	stream nextPutAll: closure copiedValues printString .
	stream nextPutAll: ' args: '.
	stream nextPutAll: closure arguments printString.

]

{ #category : #visiting }
IRPrinter >> visitPushDup: dup [
	
	stream nextPutAll: 'pushDup'



]

{ #category : #visiting }
IRPrinter >> visitPushFullClosure: fullBlock [

	stream nextPutAll: 'pushFullBlock: '.
	stream nextPutAll: fullBlock compiledBlock printString.
	stream nextPutAll: ' numCopied: '.
	stream nextPutAll: fullBlock copiedValues size printString.

]

{ #category : #visiting }
IRPrinter >> visitPushInstVar: instVar [
	
	stream nextPutAll: 'pushInstVar: '.
	instVar index printOn: stream.

]

{ #category : #visiting }
IRPrinter >> visitPushLiteral: lit [

	| object |
	object := lit literal.
	stream nextPutAll: 'pushLiteral: '.
	object isVariableBinding ifTrue: [^ stream nextPutAll: object key].
	object printOn: stream.

]

{ #category : #visiting }
IRPrinter >> visitPushLiteralVariable: var [
	
	| object |
	object := var association.
	stream nextPutAll: 'pushLiteralVariable: '.
	object isVariableBinding ifTrue: [^ stream nextPutAll: object key].
	object printOn: stream.
]

{ #category : #visiting }
IRPrinter >> visitPushReceiver: receiver [

	stream nextPutAll: 'pushReceiver'



]

{ #category : #visiting }
IRPrinter >> visitPushRemoteTemp: remoteTemp [

	stream nextPutAll: 'pushRemoteTemp: '.
	remoteTemp name printOn: stream.
	stream nextPutAll: ' inVector: '.
	remoteTemp tempVectorName printOn: stream
]

{ #category : #visiting }
IRPrinter >> visitPushTemp: tmp [

	stream nextPutAll: 'pushTemp: '.
	tmp name printOn: stream.



]

{ #category : #visiting }
IRPrinter >> visitPushThisContext: ctxt [

	stream nextPutAll: 'pushThisContext'

]

{ #category : #visiting }
IRPrinter >> visitReturn: ret [
	
	stream nextPutAll: 'returnTop'.


]

{ #category : #visiting }
IRPrinter >> visitReturnInstVar: instVar [
	stream nextPutAll: 'returnInstVar: '.
   instVar index printOn: stream.
]

{ #category : #visiting }
IRPrinter >> visitReturnLiteral: lit [

	| object |
	object := lit literal.
	stream nextPutAll: 'returnLiteral: '.
	object isVariableBinding ifTrue: [^ stream nextPutAll: object key].
	object printOn: stream.

]

{ #category : #visiting }
IRPrinter >> visitReturnReceiver: receiver [

	stream nextPutAll: 'returnReceiver'



]

{ #category : #visiting }
IRPrinter >> visitSend: send [

	send superOf
		ifNil: [ stream nextPutAll: 'send: '.
			send selector printOn: stream. ]
		ifNotNil: [ :behavior | stream nextPutAll: 'send: '.
			send selector printOn: stream.
			stream nextPutAll: ' toSuperOf: '.
			behavior printOn: stream ]


]

{ #category : #visiting }
IRPrinter >> visitSequence: instructionSequence [
	self label: instructionSequence orderNumber.
	super visitSequence: instructionSequence

]

{ #category : #visiting }
IRPrinter >> visitStoreInstVar: instVar [
	stream nextPutAll: 'storeInstVar: '.
   instVar index printOn: stream.
]

{ #category : #visiting }
IRPrinter >> visitStoreLiteralVariable: var [

	| object |
	object := var association.
	stream nextPutAll: 'storeLiteralVariable: '.
	object isVariableBinding ifTrue: [^ stream nextPutAll: object key].
	object printOn: stream.
]

{ #category : #visiting }
IRPrinter >> visitStoreRemoteTemp: remoteTemp [
	stream nextPutAll: 'storeRemoteTemp: '.
	remoteTemp name printOn: stream.
	stream nextPutAll: ' inVector: '.
	remoteTemp tempVectorName printOn: stream.
]

{ #category : #visiting }
IRPrinter >> visitStoreTemp: tmp [

   	stream nextPutAll: 'storeTemp: '.
	tmp name printOn: stream.
 
]

{ #category : #visiting }
IRPrinter >> visitTempVector: tempVector [

	stream nextPutAll: 'createTempVectorNamed: '.
	tempVector name printOn: stream.
	stream nextPutAll: ' withVars: '.
	stream nextPutAll: tempVector vars printString.
	
	
]
