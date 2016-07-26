tests - evaluating
testValueWithReceiverArguments
	
	| method value |

	method := self class compiledMethodAt: #returnTrue.

	value := method valueWithReceiver: nil arguments: #().
	self assert: (value = true).

	method := self class compiledMethodAt: #returnPlusOne:.
	value := method valueWithReceiver: nil arguments: #(1).
	self assert: (value = 2).	