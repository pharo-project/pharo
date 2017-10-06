tests
testBlockNodeIsUsed
	| tree |
	" block node is not a used-node"
	tree := self parserClass parseMethod:
			'tmp [ 3+4 ]'.
	self deny: tree body statements last isUsed.
	" even a block node with non-local return is not *used* "
	tree := self parserClass parseMethod:
	      'tmp [^ 3+4 ]'.
	self deny: tree body statements last isUsed.
	
	"an explicit return node is *used*"
	tree := self parserClass parseMethod:
			'tmp ^[ 3+4 ]'.
	self assert: tree body statements last isUsed.
	