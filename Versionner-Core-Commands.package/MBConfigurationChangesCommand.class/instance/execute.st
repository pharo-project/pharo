execute
execute
	"View the changes made in the working copy."
	
	| wc repo |
	wc := target workingCopy.
	(repo := self selectRepositoryFromWorkingCopy: wc) ifNil: [ ^self ].
	self findChangesFor: wc relativeToRepository: repo