execute
execute
	"View the changes made in the working copy."

	| wc repositoryOrSymbolicVersion |
	wc := target spec workingCopy ifNil: [ ^ self ].
	wc ancestors isEmpty
		ifTrue: [ ^ self ].
	(repositoryOrSymbolicVersion := self selectRepositoryOrSymbolicVersion: wc) ifNil: [ ^ self ].
	repositoryOrSymbolicVersion isSymbol
		ifTrue: [ self changesForSymbolicVersion: repositoryOrSymbolicVersion workingCopy: wc]
		ifFalse: [ self findChangesFor: wc relativeToRepository: repositoryOrSymbolicVersion ]