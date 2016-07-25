access to remote temps
asStorableNode: encoder
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