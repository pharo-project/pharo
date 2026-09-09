"
I interpret IRMethod instructions and write them out to a print stream.
I'm basically used to produce a precise printOn: of IRMethods.
"
Class {
	#name : 'OCIRPrinterVisitor',
	#superclass : 'OCIRVisitor',
	#instVars : [
		'stream'
	],
	#category : 'OpalCompiler-Core-IR-Manipulation',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Manipulation'
}

{ #category : 'private' }
OCIRPrinterVisitor >> label: seqNum [

	"add tab and cr since this does not get called within interpretInstruction:"
	stream cr.  "extra cr just to space out sequences"
	stream nextPutAll: 'label: '.
	seqNum printOn: stream.
	stream cr
]

{ #category : 'private' }
OCIRPrinterVisitor >> stream: stringWriteStream [

	stream := stringWriteStream
]

{ #category : 'visiting' }
OCIRPrinterVisitor >> visitBlockReturnTop: blockReturnTop [

	stream nextPutAll: 'blockReturnTop'
]

{ #category : 'visiting' }
OCIRPrinterVisitor >> visitInstruction: aNode [
	self visitNode: aNode.
	stream cr
]

{ #category : 'visiting' }
OCIRPrinterVisitor >> visitJump: jump [

	stream nextPutAll: 'goto: '.
	jump destination orderNumber printOn: stream
]

{ #category : 'visiting' }
OCIRPrinterVisitor >> visitJumpIf: jumpIf [

	stream nextPutAll: 'if: '.
	jumpIf boolean printOn: stream.
	stream nextPutAll: ' goto: '.
	jumpIf destination orderNumber printOn: stream.
	stream nextPutAll: ' else: '.
	jumpIf otherwise orderNumber printOn: stream
]

{ #category : 'visiting' }
OCIRPrinterVisitor >> visitPop: pop [

	stream nextPutAll: 'popTop'
]

{ #category : 'visiting' }
OCIRPrinterVisitor >> visitPopIntoInstVar: instVar [

	stream nextPutAll: 'popIntoInstVar: '.
	instVar index printOn: stream
]

{ #category : 'visiting' }
OCIRPrinterVisitor >> visitPopIntoLiteralVariable: litVar [

	| object |
	object := litVar association.
	stream nextPutAll: 'popIntoLiteralVariable: '.
	object isVariableBinding ifTrue: [^ stream nextPutAll: object key].
	object printOn: stream
]

{ #category : 'visiting' }
OCIRPrinterVisitor >> visitPopIntoRemoteTemp: remoteTemp [

	stream nextPutAll: 'popIntoRemoteTemp: '.
	remoteTemp name printOn: stream.
	stream nextPutAll: ' inVector: '.
	remoteTemp tempVectorName printOn: stream
]

{ #category : 'visiting' }
OCIRPrinterVisitor >> visitPopIntoTemp: tmp [

	stream nextPutAll: 'popIntoTemp: '.
	tmp name printOn: stream
]

{ #category : 'visiting' }
OCIRPrinterVisitor >> visitPushArray: array [

	array cons
		ifTrue: [
			stream nextPutAll: 'pushConsArray: ' ]
		ifFalse: [
			stream nextPutAll: 'pushNewArray: ' ].
	array size printOn: stream
]

{ #category : 'visiting' }
OCIRPrinterVisitor >> visitPushDup: dup [

	stream nextPutAll: 'pushDup'
]

{ #category : 'visiting' }
OCIRPrinterVisitor >> visitPushFullClosure: fullBlock [

	stream nextPutAll: 'pushFullBlock: '.
	stream nextPutAll: fullBlock compiledBlock printString.
	stream nextPutAll: ' numCopied: '.
	stream nextPutAll: fullBlock copiedValues size printString
]

{ #category : 'visiting' }
OCIRPrinterVisitor >> visitPushInstVar: instVar [

	stream nextPutAll: 'pushInstVar: '.
	instVar index printOn: stream
]

{ #category : 'visiting' }
OCIRPrinterVisitor >> visitPushLiteral: lit [

	| object |
	object := lit literal.
	stream nextPutAll: 'pushLiteral: '.
	object isVariableBinding ifTrue: [^ stream nextPutAll: object key].
	object printOn: stream
]

{ #category : 'visiting' }
OCIRPrinterVisitor >> visitPushLiteralVariable: var [

	| object |
	object := var association.
	stream nextPutAll: 'pushLiteralVariable: '.
	object isVariableBinding ifTrue: [^ stream nextPutAll: object key].
	object printOn: stream
]

{ #category : 'visiting' }
OCIRPrinterVisitor >> visitPushReceiver: receiver [

	stream nextPutAll: 'pushReceiver'
]

{ #category : 'visiting' }
OCIRPrinterVisitor >> visitPushRemoteTemp: remoteTemp [

	stream nextPutAll: 'pushRemoteTemp: '.
	remoteTemp name printOn: stream.
	stream nextPutAll: ' inVector: '.
	remoteTemp tempVectorName printOn: stream
]

{ #category : 'visiting' }
OCIRPrinterVisitor >> visitPushTemp: tmp [

	stream nextPutAll: 'pushTemp: '.
	tmp name printOn: stream
]

{ #category : 'visiting' }
OCIRPrinterVisitor >> visitPushThisContext: ctxt [

	stream nextPutAll: 'pushThisContext'
]

{ #category : 'visiting' }
OCIRPrinterVisitor >> visitPushThisProcess: pushThisProcess [

	stream nextPutAll: 'pushThisProcess'
]

{ #category : 'visiting' }
OCIRPrinterVisitor >> visitReturn: ret [

	stream nextPutAll: 'returnTop'
]

{ #category : 'visiting' }
OCIRPrinterVisitor >> visitReturnInstVar: instVar [
	stream nextPutAll: 'returnInstVar: '.
   instVar index printOn: stream
]

{ #category : 'visiting' }
OCIRPrinterVisitor >> visitReturnLiteral: lit [

	| object |
	object := lit literal.
	stream nextPutAll: 'returnLiteral: '.
	object isVariableBinding ifTrue: [^ stream nextPutAll: object key].
	object printOn: stream
]

{ #category : 'visiting' }
OCIRPrinterVisitor >> visitReturnReceiver: receiver [

	stream nextPutAll: 'returnReceiver'
]

{ #category : 'visiting' }
OCIRPrinterVisitor >> visitSend: send [

	send superOf
		ifNil: [ stream nextPutAll: 'send: '.
			send selector printOn: stream. ]
		ifNotNil: [ :behavior | stream nextPutAll: 'send: '.
			send selector printOn: stream.
			stream nextPutAll: ' toSuperOf: '.
			behavior printOn: stream ]
]

{ #category : 'visiting' }
OCIRPrinterVisitor >> visitSequence: instructionSequence [
	self label: instructionSequence orderNumber.
	super visitSequence: instructionSequence
]

{ #category : 'visiting' }
OCIRPrinterVisitor >> visitStoreInstVar: instVar [
	stream nextPutAll: 'storeInstVar: '.
   instVar index printOn: stream
]

{ #category : 'visiting' }
OCIRPrinterVisitor >> visitStoreLiteralVariable: var [

	| object |
	object := var association.
	stream nextPutAll: 'storeLiteralVariable: '.
	object isVariableBinding ifTrue: [^ stream nextPutAll: object key].
	object printOn: stream
]

{ #category : 'visiting' }
OCIRPrinterVisitor >> visitStoreRemoteTemp: remoteTemp [
	stream nextPutAll: 'storeRemoteTemp: '.
	remoteTemp name printOn: stream.
	stream nextPutAll: ' inVector: '.
	remoteTemp tempVectorName printOn: stream
]

{ #category : 'visiting' }
OCIRPrinterVisitor >> visitStoreTemp: tmp [

   	stream nextPutAll: 'storeTemp: '.
	tmp name printOn: stream
]

{ #category : 'visiting' }
OCIRPrinterVisitor >> visitTempVector: tempVector [

	stream nextPutAll: 'createTempVectorNamed: '.
	tempVector name printOn: stream.
	stream nextPutAll: ' withVars: '.
	stream nextPutAll: tempVector vars printString
]
