accessing
findPathTo: anObject from: fromObject 
	"self findPathTo: RefactoringManager instance from: Object"

	^(self new)
		startFrom: fromObject;
		findPath: [:each | each == anObject]