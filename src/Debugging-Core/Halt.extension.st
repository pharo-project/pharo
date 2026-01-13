Extension { #name : 'Halt' }

{ #category : '*Debugging-Core' }
Halt >> completeProcess: aProcess with: aContext [
	^ aContext methodNode
		pragmaNamed: #debuggerCompleteToSender
		ifPresent: [ aProcess completeTo: aContext sender ]
		ifAbsent: [ super completeProcess: aProcess with: aContext ]
]
