testing - primitives
testExamplePrimitiveModuleError
"teset for primitive declaration
  <primitive: 'primFunction'  module: 'primModule' error: errorCode >
"
	| method ast ir |
	method := OCOpalExamples >> #examplePrimitiveModuleError.
	ast := method parseTree.
	ir := (ast doSemanticAnalysisIn: OCOpalExamples) ir.
	self assert: ir tempKeys equals: #(#errorCode)