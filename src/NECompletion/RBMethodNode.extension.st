Extension { #name : #RBMethodNode }

{ #category : #'*NECompletion' }
RBMethodNode >> completionToken [
	^ ''
]

{ #category : #'*NECompletion' }
RBMethodNode >> doSemanticAnalysisIn: behavior [
	self methodClass: behavior.
	self doSemanticAnalysis
]
