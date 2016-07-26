querying
outgoingCommits
	| remoteBranch lines currentLine |
	
	remoteBranch := self remoteBranchName ifNil: [ IceNoRemoteBranch signal ].
	lines := OrderedCollection new.
	(self command: {'log'. 'origin/{1}..HEAD' format: {remoteBranch}. '--format=|%H|%an|%ai|%P|%s'}) linesDo: [ :line |
		(line beginsWith: '|')
			ifTrue:  [ 
				currentLine ifNotNil: [ lines add: currentLine contents ].
				currentLine := WriteStream with: line ]
			ifFalse: [ 
				currentLine cr; nextPutAll: line ]].

	currentLine ifNotNil: [ lines add: currentLine contents ].
	
	^ lines collect: [ :line | IceCommitInfo new initializeFrom: line ]
