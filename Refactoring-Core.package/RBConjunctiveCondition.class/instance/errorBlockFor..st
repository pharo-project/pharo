private
errorBlockFor: aBoolean 
	^aBoolean
		ifTrue: [nil]
		ifFalse: 
			[failed = #leftFailed ifTrue: [left errorBlock] ifFalse: [right errorBlock]]