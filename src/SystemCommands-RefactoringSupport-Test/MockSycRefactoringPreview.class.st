Class {
	#name : #MockSycRefactoringPreview,
	#superclass : #SycRefactoringPreview,
	#category : #'SystemCommands-RefactoringSupport-Test'
}

{ #category : #accessing }
MockSycRefactoringPreview >> activeRBEnvironment [
	^ self
]

{ #category : #private }
MockSycRefactoringPreview >> buildDiffFor: aChange [
	"i override this method bacause i just want to test if the transmission use this method"
	^ 'useForTest'
]

{ #category : #accessing }
MockSycRefactoringPreview >> selectedRefactorings [
	^ selectedRefactorings
]
