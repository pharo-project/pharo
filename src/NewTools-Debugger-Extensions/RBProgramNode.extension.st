Extension { #name : #RBProgramNode }

{ #category : #'*NewTools-Debugger-Extensions' }
RBProgramNode >> allBreakpointLinks [
	^ self links select: [ :link | link metaObject = Break ]
]

{ #category : #'*NewTools-Debugger-Extensions' }
RBProgramNode >> enableHalts: aBoolean [
	^self propertyAt: #enabledForHaltOrBreakpoint put: aBoolean
]

{ #category : #'*NewTools-Debugger-Extensions' }
RBProgramNode >> enabledForHaltOrBreakpoint [
	^self propertyAt: #enabledForHaltOrBreakpoint ifAbsent: [ true ]
]
