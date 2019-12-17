Extension { #name : #DTTestCoverage }

{ #category : #'*Deprecated80' }
DTTestCoverage >> oldPragmaForResultTrees [
	"This is the symbol for the old pragma, in near future, it will not be supported anymore. Please update your code to use the pragma returned by #pragmaForResultTrees."
	^ #'dTTestCoverageResultTreeNamed:'
]
