Extension { #name : #DebugSession }

{ #category : #'*NewTools-Debugger-Extensions' }
DebugSession >> browse: aContext [

	aContext method browse
]

{ #category : #'*NewTools-Debugger-Extensions' }
DebugSession >> shortStack [
	^self stackOfSize: self shortStackSize
]

{ #category : #'*NewTools-Debugger-Extensions' }
DebugSession >> shortStackSize [
	^ 20
]
