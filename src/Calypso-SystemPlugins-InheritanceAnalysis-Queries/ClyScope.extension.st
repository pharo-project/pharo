Extension { #name : 'ClyScope' }

{ #category : '*Calypso-SystemPlugins-InheritanceAnalysis-Queries' }
ClyScope >> inheritanceAnalyzer [

	^ self environment getPlugin: ClyInheritanceAnalysisEnvironmentPlugin
]
