instruction stream support
superSendScanBlockUsing: scanner
	"Answer a block argument for InstructionStream>>scanFor:
	 that answers true for super sends."

	"*	224		11100000	aaaaaaaa	Extend A (Ext A = Ext A prev * 256 + Ext A)
	 *	225		11100001	sbbbbbbb	Extend B (Ext B = Ext B prev * 256 + Ext B)
	 **	235		11101011	iiiiijjj		Send To Superclass Literal Selector #iiiii (+ Extend A * 32) with jjj (+ Extend B * 8) Arguments"
					
	^[:instr | instr = 235]