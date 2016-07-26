testing
evaluatedFirst: aNode 
	self children do: 
			[:each | 
			each == aNode ifTrue: [^true].
			each isImmediateNode ifFalse: [^false]].
	^false