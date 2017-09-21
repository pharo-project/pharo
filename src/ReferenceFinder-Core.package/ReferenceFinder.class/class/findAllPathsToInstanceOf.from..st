accessing
findAllPathsToInstanceOf: aBehavior from: fromObject 
	"self findAllPathsToInstanceOf: RefactoringBrowser from: ScheduledControllers"

	^(self new)
		startFrom: fromObject;
		findAllPaths: [:each | each class == aBehavior]