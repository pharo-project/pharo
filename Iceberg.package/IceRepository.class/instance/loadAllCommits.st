private
loadAllCommits
	| currentLine |
	commits := Dictionary new.
	
	(self command: 'log --format=|%H|%an|%ai|%P|%s --reverse') linesDo: [ :line |
		(line beginsWith: '|')
			ifTrue:  [ 
				currentLine ifNotNil: [ IceCommitInfo createIn: self from: currentLine contents ].
				currentLine := WriteStream with: line ]
			ifFalse: [ 
				currentLine cr; nextPutAll: line ]].
		
	currentLine ifNotNil: [ IceCommitInfo createIn: self from: currentLine contents ].
	
	^ commits
