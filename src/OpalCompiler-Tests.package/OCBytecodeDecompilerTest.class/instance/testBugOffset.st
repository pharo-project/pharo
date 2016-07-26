tests
testBugOffset
	| iRMethod aCompiledMethod ir method |
	iRMethod := IRBuilder new
		addTemps: #(#a #b);
		pushTemp: #b;
		pushTemp: #a;
		returnTop;
		ir.
	aCompiledMethod := iRMethod compiledMethod.
	ir := aCompiledMethod decompileIR.
	method := ir compiledMethod.
	self assert: aCompiledMethod symbolic equals: method symbolic