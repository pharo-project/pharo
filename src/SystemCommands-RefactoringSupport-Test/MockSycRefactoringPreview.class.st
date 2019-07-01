Class {
	#name : #MockSycRefactoringPreview,
	#superclass : #SycRefactoringPreview,
	#instVars : [
		'title'
	],
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
	| mockTreeTableItem2 mockTreeTableItem3 mockTreeTableItem1 |
	mockTreeTableItem1 := MockTreeTableItem title: 'mockTreeTableItem1'.
	mockTreeTableItem2 := MockTreeTableItem title: 'mockTreeTableItem2'.
	mockTreeTableItem3 := MockTreeTableItem title: 'mockTreeTableItem3'.
	table
		items:
			{mockTreeTableItem1.
			mockTreeTableItem2.
			mockTreeTableItem3}
]
