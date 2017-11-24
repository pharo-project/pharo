"
I interpret IRMethod instructions and write them out to a print stream.
"
Class {
	#name : #IRPrinterV2,
	#superclass : #IRVisitor,
	#instVars : [
		'stream'
	],
	#category : #'OpalCompiler-Core-IR-Manipulation'
}

{ #category : #private }
IRPrinterV2 >> label: seqNum [

	"add tab and cr since this does not get called within interpretInstruction:"
	stream cr.  "extra cr just to space out sequences"
	stream nextPutAll: 'label: '.
	seqNum printOn: stream.
	stream cr.

]

{ #category : #private }
IRPrinterV2 >> stream: stringWriteStream [

	stream := stringWriteStream
]

{ #category : #visiting }
IRPrinterV2 >> visitBlockReturnTop: blockReturnTop [

	stream nextPutAll: 'blockReturnTop'.


]

{ #category : #visiting }
IRPrinterV2 >> visitInstruction: aNode [
	self visitNode: aNode.
	stream cr.
]

{ #category : #visiting }
IRPrinterV2 >> visitJump: jump [

	stream nextPutAll: 'goto: '.
	jump destination orderNumber printOn: stream.

]

{ #category : #visiting }
IRPrinterV2 >> visitJumpIf: jumpIf [

	stream nextPutAll: 'if: '.
	jumpIf boolean printOn: stream.
	stream nextPutAll: ' goto: '.
	jumpIf destination orderNumber printOn: stream.
	stream nextPutAll: ' else: '.
	jumpIf otherwise orderNumber printOn: stream.

]

{ #category : #visiting }
IRPrinterV2 >> visitPop: pop [

	stream nextPutAll: 'popTop'

]

{ #category : #visiting }
IRPrinterV2 >> visitPopIntoInstVar: instVar [
	
	stream nextPutAll: 'popIntoInstVar: '.
	instVar index printOn: stream.

]

{ #category : #visiting }
IRPrinterV2 >> visitPopIntoLiteralVariable: litVar [
	
	| object |
	object := litVar association.
	stream nextPutAll: 'popIntoLiteralVariable: '.
	object isVariableBinding ifTrue: [^ stream nextPutAll: object key].
	object printOn: stream.
]

{ #category : #visiting }
IRPrinterV2 >> visitPopIntoRemoteTemp: remoteTemp [

	stream nextPutAll: 'popIntoRemoteTemp: '.
	remoteTemp name printOn: stream.
	stream nextPutAll: ' inVector: '.
	remoteTemp tempVectorName printOn: stream
]

{ #category : #visiting }
IRPrinterV2 >> visitPopIntoTemp: tmp [

	stream nextPutAll: 'popIntoTemp: '.
	tmp name printOn: stream.



]

{ #category : #visiting }
IRPrinterV2 >> visitPushArray: array [
	
	array cons 
		ifTrue: [  
			stream nextPutAll: 'pushConsArray: ' ]
		ifFalse: [
			stream nextPutAll: 'pushNewArray: ' ].
	array size printOn: stream. 

]

{ #category : #visiting }
IRPrinterV2 >> visitPushClosureCopy: closure [

	stream nextPutAll: 'pushClosureCopyCopiedValues: '.
	stream nextPutAll: closure copiedValues printString .
	stream nextPutAll: ' args: '.
	stream nextPutAll: closure arguments printString.

]

{ #category : #visiting }
IRPrinterV2 >> visitPushDup: dup [
	
	stream nextPutAll: 'pushDup'



]

{ #category : #visiting }
IRPrinterV2 >> visitPushFullClosure: fullBlock [

	stream nextPutAll: 'pushFullBlock: '.
	stream nextPutAll: fullBlock compiledBlock printString.
	stream nextPutAll: ' numCopied: '.
	stream nextPutAll: fullBlock copiedValues size printString.

]

{ #category : #visiting }
IRPrinterV2 >> visitPushInstVar: instVar [
	
	stream nextPutAll: 'pushInstVar: '.
	instVar index printOn: stream.

]

{ #category : #visiting }
IRPrinterV2 >> visitPushLiteral: lit [

	| object |
	object := lit literal.
	stream nextPutAll: 'pushLiteral: '.
	object isVariableBinding ifTrue: [^ stream nextPutAll: object key].
	object printOn: stream.

]

{ #category : #visiting }
IRPrinterV2 >> visitPushLiteralVariable: var [
	
	| object |
	object := var association.
	stream nextPutAll: 'pushLiteralVariable: '.
	object isVariableBinding ifTrue: [^ stream nextPutAll: object key].
	object printOn: stream.
]

{ #category : #visiting }
IRPrinterV2 >> visitPushReceiver: receiver [

	stream nextPutAll: 'pushReceiver'



]

{ #category : #visiting }
IRPrinterV2 >> visitPushRemoteTemp: remoteTemp [

	stream nextPutAll: 'pushRemoteTemp: '.
	remoteTemp name printOn: stream.
	stream nextPutAll: ' inVector: '.
	remoteTemp tempVectorName printOn: stream
]

{ #category : #visiting }
IRPrinterV2 >> visitPushTemp: tmp [

	stream nextPutAll: 'pushTemp: '.
	tmp name printOn: stream.



]

{ #category : #visiting }
IRPrinterV2 >> visitPushThisContext: ctxt [

	stream nextPutAll: 'pushThisContext'

]

{ #category : #visiting }
IRPrinterV2 >> visitReturn: ret [
	
	stream nextPutAll: 'returnTop'.


]

{ #category : #visiting }
IRPrinterV2 >> visitReturnInstVar: instVar [
	stream nextPutAll: 'returnInstVar: '.
   instVar index printOn: stream.
]

{ #category : #visiting }
IRPrinterV2 >> visitReturnLiteral: lit [

	| object |
	object := lit literal.
	stream nextPutAll: 'returnLiteral: '.
	object isVariableBinding ifTrue: [^ stream nextPutAll: object key].
	object printOn: stream.

]

{ #category : #visiting }
IRPrinterV2 >> visitReturnReceiver: receiver [

	stream nextPutAll: 'returnReceiver'



]

{ #category : #visiting }
IRPrinterV2 >> visitSend: send [

	send superOf
		ifNil: [ stream nextPutAll: 'send: '.
			send selector printOn: stream. ]
		ifNotNil: [ :behavior | stream nextPutAll: 'send: '.
			send selector printOn: stream.
			stream nextPutAll: ' toSuperOf: '.
			behavior printOn: stream ]


]

{ #category : #visiting }
IRPrinterV2 >> visitSequence: instructionSequence [
	self label: instructionSequence orderNumber.
	super visitSequence: instructionSequence

]

{ #category : #visiting }
IRPrinterV2 >> visitStoreInstVar: instVar [
	stream nextPutAll: 'storeInstVar: '.
   instVar index printOn: stream.
]

{ #category : #visiting }
IRPrinterV2 >> visitStoreLiteralVariable: var [

	| object |
	object := var association.
	stream nextPutAll: 'storeLiteralVariable: '.
	object isVariableBinding ifTrue: [^ stream nextPutAll: object key].
	object printOn: stream.
]

{ #category : #visiting }
IRPrinterV2 >> visitStoreRemoteTemp: remoteTemp [
	stream nextPutAll: 'storeRemoteTemp: '.
	remoteTemp name printOn: stream.
	stream nextPutAll: ' inVector: '.
	remoteTemp tempVectorName printOn: stream.
]

{ #category : #visiting }
IRPrinterV2 >> visitStoreTemp: tmp [

   	stream nextPutAll: 'storeTemp: '.
	tmp name printOn: stream.
 
]

{ #category : #visiting }
IRPrinterV2 >> visitTempVector: tempVector [

	stream nextPutAll: 'createTempVectorNamed: '.
	tempVector name printOn: stream.
	stream nextPutAll: ' withVars: '.
	stream nextPutAll: tempVector vars printString.
	
	
]
