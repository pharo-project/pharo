I am a helper class to simplify tests related to scopes. I collect all scopes of the AST.
This way one can easily check for scopes and variables even when the scopes are deeply nested.
Simple example (with just a method scope):

|ast scopes ivar|
ast := (OCOpalExamples>>#exampleiVar) 
		parseTree doSemanticAnalysisIn: OCOpalExamples.
scopes := (OCScopesCollector new visitNode: ast) scopes.
ivar := scopes first lookupVar: #iVar