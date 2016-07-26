testing - temp access
testExampleTempNamedPutCopying2
"modifying a copied temp variable will *not* modify the value in the outer context. 
(See other test case OCClosureCompilerTest>>#testDebuggerTempAccess and notes on fogbugz issue 17702"
	self assert: (self compileAndRunExample: #exampleTempNamedPutCopying2) equals: 1