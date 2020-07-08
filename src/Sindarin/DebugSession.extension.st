Extension { #name : #DebugSession }

{ #category : #'*Sindarin' }
DebugSession >> asSindarinDebugSession [
	^ SindarinDebugSession new debugSession: self
]
