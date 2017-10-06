accessing
findPathToInstanceOf: aBehavior from: fromObject 
	"self findPathToInstanceOf: RefactoringManager from: Object"

	^(self new)
		startFrom: fromObject;
		findPath: [:each | each class == aBehavior]