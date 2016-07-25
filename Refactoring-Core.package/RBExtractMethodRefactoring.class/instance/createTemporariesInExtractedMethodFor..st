transforming
createTemporariesInExtractedMethodFor: assigned 
	assigned do: [:each | extractedParseTree body addTemporaryNamed: each]