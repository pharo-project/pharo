Extension { #name : #NautilusUI }

{ #category : #'*NautilusRefactoring' }
NautilusUI >> basicRenameClass: aClass [
	" Override to set the class rename with RB by default "

	self renameClassWithRefactoringEngine: aClass
]

{ #category : #'*NautilusRefactoring' }
NautilusUI >> refactor [

	^ NautilusRefactoring model: self
]

{ #category : #'*NautilusRefactoring' }
NautilusUI >> refactorCode: aText [
	acceptor := CodeRefactoringAcceptor model: self.
	self sourceCode: aText
]

{ #category : #'*NautilusRefactoring' }
NautilusUI >> removeClassWithRefactoringEngine: aCollection [

	^ self refactor removeClasses: aCollection 
]

{ #category : #'*NautilusRefactoring' }
NautilusUI >> renameClassWithRefactoringEngine: aClass [

	self refactor renameClass: aClass 
]

{ #category : #'*NautilusRefactoring' }
NautilusUI >> rewriteCode: aText [

	acceptor := CodeRewritingAcceptor model: self.
	self sourceCode: aText
]

{ #category : #'*NautilusRefactoring' }
NautilusUI >> searchCode: aText [
	acceptor := CodeSearchingAcceptor model: self.
	self setStylerClass: nil.
	self pendingText: aText
]
