Class {
	#name : 'RBRenameTemporaryRefactoringTest',
	#superclass : 'RBAbstractRefactoringTest',
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'failure tests' }
RBRenameTemporaryRefactoringTest >> testFailureBadIntervalAndVariableDoesNotExist [

	model := self modelOnClasses: { RBClassDataForRefactoringTest }.

	self shouldFail:
		(ReRenameArgumentOrTemporaryRefactoring
    			model: model
    			renameTemporaryFrom: (14 to: 17)
    			to: 'asdf'
    			in: #RBClassDataForRefactoringTest
    			selector: #testMethod).
	self shouldFail:
		(ReRenameArgumentOrTemporaryRefactoring
    			model: model
    			renameTemporaryFrom: (17 to: 14)
    			to: 'asdf'
    			in: #RBClassDataForRefactoringTest
    			selector: #testMethod).
	self shouldFail:
		(ReRenameArgumentOrTemporaryRefactoring
    			model: model
    			renameTemporaryFrom: (19 to: 24)
    			to: 'asdf'
    			in: #RBClassDataForRefactoringTest
    			selector: #testMethod)
]

{ #category : 'failure tests' }
RBRenameTemporaryRefactoringTest >> testFailureBadName [
	model := self modelOnClasses: { RBLintRuleTestData }.
				
	self
		shouldFail:
			(ReRenameArgumentOrTemporaryRefactoring
                model: model
                renameTemporaryFrom: (15 to: 19)
                to: 'DependentFields'
                in: #RBLintRuleTestData
                selector: #openEditor);
		shouldFail:
			(ReRenameArgumentOrTemporaryRefactoring
                model: model
                renameTemporaryFrom: (15 to: 19)
                to: 'a b'
                in: #RBLintRuleTestData
                selector: #openEditor)
]

{ #category : 'failure tests' }
RBRenameTemporaryRefactoringTest >> testFailureExistingTempVariable [

	| refactoring |
	refactoring := ReRenameArgumentOrTemporaryRefactoring
            				model: model
            				renameTemporaryFrom: (15 to: 19)
            				to: 'rules'
            				in: #RBLintRuleTestData
            				selector: #openEditor.
	self shouldFail: refactoring
]

{ #category : 'failure tests' }
RBRenameTemporaryRefactoringTest >> testFailureModelBadName [

	| class |
	model := self modelOnClasses: { Object }.
	class := model classNamed: #Object.
	class
		compile:
		'aMethod: temp1 ^[| temp2 | temp2 := [:temp3 | temp3 = 5] value: 5. temp2] value'
		classified: #( #accessing ).
	self
		shouldFail: (
				ReRenameArgumentOrTemporaryRefactoring
                model: model
                renameTemporaryFrom: (20 to: 24)
                to: 'temp3'
                in: #Object
                selector: #aMethod:);
		shouldFail: (
				ReRenameArgumentOrTemporaryRefactoring
                model: model
                renameTemporaryFrom: (20 to: 24)
                to: 'temp1'
                in: #Object
                selector: #aMethod:)
]

{ #category : 'failure tests' }
RBRenameTemporaryRefactoringTest >> testFailureModelExistingVariable [

	| transformation |
	transformation := (RBAddMethodTransformation
		                   sourceCode: 'foo
									| temp bar |
									bar := 5.
									temp := bar * bar.
									Transcript show: temp printString; cr.
									^temp * temp'
		                   in: self class name
		                   withProtocol: #accessing ).
	transformation generateChanges.

	self shouldFail: (
			ReRenameArgumentOrTemporaryRefactoring
            model: transformation model
            renameTemporaryFrom: (16 to: 19)
            to: 'bar'
            in: self class name
            selector: #foo).
]

{ #category : 'tests' }
RBRenameTemporaryRefactoringTest >> testRenameArg [
	| refactoring |
	model := self modelOnClasses: { RBLintRuleTestData }.
	
	refactoring := 
			ReRenameArgumentOrTemporaryRefactoring
            model: model
            renameTemporaryFrom: (7 to: 13)
            to: 'asdf'
            in: #RBLintRuleTestData
            selector: #name:.
	self executeRefactoring: refactoring.

	self assert: ((refactoring model classNamed: #RBLintRuleTestData) parseTreeForSelector: #name:) equals: (self parseMethod: 'name: asdf
	name := asdf')
]

{ #category : 'tests' }
RBRenameTemporaryRefactoringTest >> testRenameTemporary [
	| refactoring |
	model := self modelOnClasses: { RBLintRuleTestData }.
	
	refactoring := 
			ReRenameArgumentOrTemporaryRefactoring
            model: model
            renameTemporaryFrom: (15 to: 19)
            to: 'asdf'
            in: #RBLintRuleTestData
            selector: #openEditor.
	self executeRefactoring: refactoring.

	self assert: ((refactoring model classNamed: #RBLintRuleTestData) parseTreeForSelector: #openEditor) equals: (self parseMethod: 'openEditor
								| asdf |
								asdf := self failedRules.
								asdf isEmpty ifTrue: [^self].
								asdf size == 1 ifTrue: [^asdf first viewResults]')
]

{ #category : 'tests' }
RBRenameTemporaryRefactoringTest >> testWarningExistingInstanceVariable [

	| refactoring |
	model := self modelOnClasses: { RBLintRuleTestData }.

	refactoring := ReRenameArgumentOrTemporaryRefactoring
		               model: model
		               renameTemporaryFrom: (15 to: 19)
		               to: 'name'
		               in: #RBLintRuleTestData
		               selector: #openEditor.
	self shouldWarn: refactoring.

	refactoring := ReRenameArgumentOrTemporaryRefactoring
		               model: model
		               rename: 'rules'
		               to: 'name'
		               in: #RBLintRuleTestData
		               selector: #openEditor.
	self shouldWarn: refactoring
]
