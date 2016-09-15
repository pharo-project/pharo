visitor
visitRefactoring: anEvent 
	"FIXME"
	
	^ anEvent canBuildRBRefactoring
		ifTrue: [
			(self visitEvent: anEvent)
				add: self applyCompleteRefactoringMenuAction;
				add: self applyPropagateRefactoringMenuAction;
				yourself ]
		ifFalse: [ 
			(self visitEvent: anEvent) ]