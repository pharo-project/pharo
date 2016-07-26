code generation
sizeCodeForBlockValue: encoder
	"Answer the size for evaluating the last statement in a block"
	^self sizeCodeForValue: encoder