Extension { #name : #StDebugger }

{ #category : #'*NewTools-Sindarin-Tools' }
StDebugger >> sindarinDebugger [
	^ SindarinDebugger attachTo: self session
]
