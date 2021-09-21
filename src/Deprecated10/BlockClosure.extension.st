Extension { #name : #BlockClosure }

{ #category : #'*Deprecated10' }
BlockClosure >> valueAt: blockPriority [ 
	"Evaluate the receiver (block), with another priority as the actual one 
	and restore it afterwards. The caller should be careful with using 
	higher priorities."
	| activeProcess result outsidePriority |
	
	self deprecated: 'This should not be used. If you want to execute in a different priority use a different process. Maybe you don''t want to do that neither'.
	
	activeProcess := Processor activeProcess.
	outsidePriority := activeProcess priority.
	activeProcess priority: blockPriority.
	result := self ensure: [activeProcess priority: outsidePriority].
	"Yield after restoring lower priority to give the preempted processes a  
	chance to run."
	blockPriority > outsidePriority
		ifTrue: [Processor yield].
	^ result
]
