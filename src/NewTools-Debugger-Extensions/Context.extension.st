Extension { #name : #Context }

{ #category : #'*NewTools-Debugger-Extensions' }
Context >> debuggerInspectorTempsNodes [
	
	^  (StNodeCollector for: self) collectTempsNodes
]
