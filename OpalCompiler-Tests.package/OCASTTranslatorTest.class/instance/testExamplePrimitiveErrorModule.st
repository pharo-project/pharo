testing - primitives
testExamplePrimitiveErrorModule
"test for primitive declaration
  <primitive: 'primFunction'  error: errorCode module: 'primModule'  >
"
	| method ast ir |
	method := OCOpalExamples >> #examplePrimitiveErrorModule.
	ast := method parseTree.
	ir := (ast doSemanticAnalysisIn: OCOpalExamples) ir.
	self assert: ir tempKeys equals: #(#errorCode)