Extension { #name : 'PseudoVariable' }

{ #category : '*OpalCompiler-Core' }
PseudoVariable >> asDoItVariableFrom: aContext [
	^ OCDoItVariable fromContext: aContext variable: self
]
