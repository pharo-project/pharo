visitor
visitRefactoring: anEvent 
	"FIXME"
	
	^ anEvent canBuildRBRefactoring
		ifTrue: [
			(self visitEvent: anEvent)
				add: self redoSelectionCompleteRefactoringMenuAction;
				add: self redoSelectionPropagateRefactoringMenuAction;
				yourself ]
		ifFalse: [ 
			(self visitEvent: anEvent) ]