Extension { #name : #ClyScope }

{ #category : #'*Calypso-SystemPlugins-InheritanceAnalysis-Queries' }
ClyScope >> inheritanceAnalyzer [

	^environment getPlugin: ClyInheritanceAnalysisEnvironmentPlugin 
]
