testing
hasConflictWithBaselineSpec: projectSpec
	projectSpec name = self name
		ifFalse: [ ^ true ].
	projectSpec project configuration className = self project configuration className
		ifFalse: [ ^ true ].
	^ ((projectSpec repositories isEmpty or: [ self repositories isEmpty ])
		or: [ projectSpec repositories hasNoLoadConflicts: self repositories ]) not