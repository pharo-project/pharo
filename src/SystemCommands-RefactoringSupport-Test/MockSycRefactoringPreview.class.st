Class {
	#name : #MockSycRefactoringPreview,
	#superclass : #SycRefactoringPreview,
	#category : #'SystemCommands-RefactoringSupport-Test'
}

{ #category : #private }
MockSycRefactoringPreview >> buildDiffFor: aChange [
	"i override this method bacause i just want to test if the transmission use this method"
	^ 'useForTest'
]

{ #category : #update }
MockSycRefactoringPreview >> generateChanges [
	"do nothing"
	
]

{ #category : #accessing }
MockSycRefactoringPreview >> selectedRefactorings [
	^ selectedRefactorings
]

{ #category : #update }
MockSycRefactoringPreview >> updateTablePresenter [
	table
		items:
			{(MockTreeTableItem title: 'mockTreeTableItem1').
			(MockTreeTableItem title: 'mockTreeTableItem2').
			(MockTreeTableItem title: 'mockTreeTableItem3')}
]
