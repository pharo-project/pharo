visiting
visitLiteralArrayNode: aRBLiteralArrayNode 
	aRBLiteralArrayNode contents do: [:each | self visitNode: each]