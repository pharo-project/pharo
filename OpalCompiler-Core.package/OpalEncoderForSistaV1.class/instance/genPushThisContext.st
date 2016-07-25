bytecode generation
genPushThisContext
	"82			01010010			Push thisContext, (then e.g. Extend B 1 = push thisProcess)"
	stream nextPut: 82