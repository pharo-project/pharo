"
This node represents accesses to temporary variables for do-its in the debugger.  Since they execute in another context, they must send a message to the original context to access the value of the temporary variable in that context.
"
Class {
	#name : #MessageAsTempNode,
	#superclass : #MessageNode,
	#category : #'Compiler-ParseNodes'
}

{ #category : #'access to remote temps' }
MessageAsTempNode >> asStorableNode: encoder [
	"This node is a message masquerading as a temporary variable.
	It currently has the form {homeContext tempAt: offset}.
	We need to generate code for {expr storeAt: offset inTempFrame: homeContext},
	where the expr, the block argument, is already on the stack.
	This, in turn will get turned into {homeContext tempAt: offset put: expr}
	at runtime if nobody disturbs storeAt:inTempFrame: in Object (not clean)"
	^ MessageAsTempNode new
		receiver: nil  "suppress code generation for reciever already on stack"
		selector: #storeAt:inTempFrame:
		arguments: (arguments copyWith: receiver)
		precedence: precedence
		from: encoder
]

{ #category : #'access to remote temps' }
MessageAsTempNode >> code [
	"Allow synthetic temp nodes to be sorted by code"
	^ arguments first literalValue
]

{ #category : #'code generation' }
MessageAsTempNode >> emitCodeForStorePop: stack encoder: encoder [
	"This node has the form {expr storeAt: offset inTempFrame: homeContext},
	where the expr, the block argument, is already on the stack."
	^self emitCodeForEffect: stack encoder: encoder
]

{ #category : #'code generation' }
MessageAsTempNode >> sizeCodeForStorePop: encoder [
	"This node has the form {expr storeAt: offset inTempFrame: homeContext},
	where the expr, the block argument, is already on the stack."
	^self sizeCodeForEffect: encoder
]

{ #category : #'access to remote temps' }
MessageAsTempNode >> store: expr from: encoder [ 
	"ctxt tempAt: n -> ctxt tempAt: n put: expr (see Assignment).
	For assigning into temps of a context being debugged."

	selector key ~= #namedTempAt: 
		ifTrue: [^self error: 'cant transform this message'].
	^ MessageAsTempNode new
		receiver: receiver
		selector: #namedTempAt:put:
		arguments: (arguments copyWith: expr)
		precedence: precedence
		from: encoder
]
