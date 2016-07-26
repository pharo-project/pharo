private
previousStepPosition
	^characterType = #eof 
		ifTrue: [stream position]
		ifFalse: [stream position - 1]