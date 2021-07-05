Class {
	#name : #MockSycRefactoringPreviewPresenter,
	#superclass : #SycRefactoringPreviewPresenter,
	#category : #'SystemCommands-RefactoringSupport-Tests'
}

{ #category : #accessing }
MockSycRefactoringPreviewPresenter >> activeRBEnvironment [
	^ self
]

{ #category : #private }
MockSycRefactoringPreviewPresenter >> buildDiffFor: aChange [
	"i override this method because i just want to test if the transmission use this method"
	^ 'useForTest'
]

{ #category : #accessing }
MockSycRefactoringPreviewPresenter >> selectedRefactorings [
	^ selectedRefactorings
]
