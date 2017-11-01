Extension { #name : #RBProgramNode }

{ #category : #'*Reflectivity-Tools-Watchpoints' }
RBProgramNode >> counter [
	
	self hasExecutionCounter ifFalse:[^nil].
	^(self beforeLinks detect: [ :link | link metaObject class = ExecutionCounter]) metaObject
]

{ #category : #'*Reflectivity-Tools' }
RBProgramNode >> hasBreakpoint [
	
	self hasMetalinkBefore ifFalse: [ ^false ].
	^self beforeLinks anySatisfy: [ :link | link metaObject = Break ]
]

{ #category : #'*Reflectivity-Tools-Watchpoints' }
RBProgramNode >> hasExecutionCounter [
	
	self hasMetalinkBefore ifFalse: [ ^false ].
	^self beforeLinks anySatisfy: [ :link | link metaObject class = ExecutionCounter ]
]

{ #category : #'*Reflectivity-Tools-Watchpoints' }
RBProgramNode >> hasWatchpoint [
	
	self hasMetalinkAfter ifFalse: [ ^false ].
	^self afterLinks anySatisfy: [ :link | link metaObject class = Watchpoint ]
]

{ #category : #'*Reflectivity-Tools-Watchpoints' }
RBProgramNode >> watchpoint [
	
	self hasWatchpoint ifFalse:[^nil].
	^(self afterLinks detect: [ :link | link metaObject class = Watchpoint ]) metaObject
]
