accessing
spec
	"Expected to be a MetacelloVersionSpec"
	
	(spec == nil  and: [  self loader ~~ nil ]) 
		ifTrue: [ 
			^[ self loader spec versionSpec ] 
				on: MessageNotUnderstood
				do: [:ex | ex return: self loader spec ]].
	^spec