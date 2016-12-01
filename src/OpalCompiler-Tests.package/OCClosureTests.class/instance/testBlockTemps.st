testing
testBlockTemps
	
	| block block1 block2 |
	"Regression test: Bytecode offset of IR was to last byte of IR node, which for blocks include temp initialization bytes. This caused scan for block creation bytecode to fail when there were many block temps, and no source node to be found."
	block := [ :arg | | a b c d e f g | a:=b:=c:=d:=e:=f:=g := arg. [ a ] ].
	self assert: block sourceNode isBlock.
	self assert: block argumentNames equals: #(#arg).
	block1 := block value: 1.
	block2 := block value: 2.
	self assert: block1 value = 1.
	self assert: block2 value = 2