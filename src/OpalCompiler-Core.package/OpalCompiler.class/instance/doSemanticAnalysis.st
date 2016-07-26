private
doSemanticAnalysis
	^[ast doSemanticAnalysisInContext: self compilationContext] 
		on: OCSemanticError 
		do: [ :ex | ex defaultAction. ^ self compilationContext failBlock value ]