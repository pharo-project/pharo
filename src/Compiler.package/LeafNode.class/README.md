I represent a leaf node of the compiler parse tree. I am abstract.
	
Types (defined in class ParseNode):
	1 LdInstType (which uses class VariableNode)
	2 LdTempType (which uses class VariableNode)
	3 LdLitType (which uses class LiteralNode)
	4 LdLitIndType (which uses class VariableNode)
	5 SendType (which uses class SelectorNode).

Note that Squeak departs slightly from the Blue Book bytecode spec.

In order to allow access to more than 63 literals and instance variables,
bytecode 132 has been redefined as DoubleExtendedDoAnything:
		byte2				byte3			Operation
(hi 3 bits)  (lo 5 bits)
	0		nargs			lit index			Send Literal Message 0-255
	1		nargs			lit index			Super-Send Lit Msg 0-255
	2		ignored			rcvr index		Push Receiver Variable 0-255
	3		ignored			lit index			Push Literal Constant 0-255
	4		ignored			lit index			Push Literal Variable 0-255
	5		ignored			rcvr index		Store Receiver Variable 0-255
	6		ignored			rcvr index		Store-pop Receiver Variable 0-255
	7		ignored			lit index			Store Literal Variable 0-255

	This has allowed bytecode 134 also to be redefined as a second extended send
	that can access literals up to 64 for nargs up to 3 without needing three bytes.
	It is just like 131, except that the extension byte is aallllll instead of aaalllll,
	where aaa are bits of argument count, and lll are bits of literal index.