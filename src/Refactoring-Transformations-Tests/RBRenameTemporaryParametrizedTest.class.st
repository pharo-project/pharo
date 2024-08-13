Class {
	#name : 'RBRenameTemporaryParametrizedTest',
	#superclass : 'RBWithDifferentsArgumentsParametrizedTest',
	#category : 'Refactoring-Transformations-Tests-Parametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Parametrized'
}

{ #category : 'tests' }
RBRenameTemporaryParametrizedTest class >> testParameters [

	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBRenameArgumentOrTemporaryRefactoring .
					  #constructor -> #renameTemporaryFrom:to:in:selector: .
					  #index -> 1};
		addCase: { #rbClass -> RBRenameTemporaryVariableTransformation .
					  #constructor -> #rename:to:in:selector: .
					  #index -> 2};
		yourself
]

{ #category : 'failure tests' }
RBRenameTemporaryParametrizedTest >> testFailureBadIntervalAndVariableDoesNotExist [

	model := self modelOnClasses: { RBClassDataForRefactoringTest }.

	self shouldFail:
		(self createRefactoringWithModel: model andArguments: {
				 {
					 (14 to: 17).
					 #rule }.
				 'asdf'.
				 #RBClassDataForRefactoringTest.
				 #testMethod }).
	self shouldFail:
		(self createRefactoringWithModel: model andArguments: {
				 {
					 (17 to: 14).
					 #xyz }.
				 'asdf'.
				 #RBClassDataForRefactoringTest.
				 #testMethod }).
	self shouldFail:
		(self createRefactoringWithModel: model andArguments: {
				 {
					 (19 to: 24).
					 #temporaryVariable1 }.
				 'asdf'.
				 #RBClassDataForRefactoringTest.
				 #testMethod })
]

{ #category : 'failure tests' }
RBRenameTemporaryParametrizedTest >> testFailureBadName [
	model := self modelOnClasses: { RBLintRuleTestData }.
				
	self
		shouldFail:
			(self createRefactoringWithModel: model andArguments: { { (15 to: 19) . #rules }.
				'DependentFields' . #RBLintRuleTestData . #openEditor });
		shouldFail:
			(self createRefactoringWithModel: model andArguments: { { (15 to: 19) . #rules }.
				'a b' . #RBLintRuleTestData . #openEditor })
]

{ #category : 'failure tests' }
RBRenameTemporaryParametrizedTest >> testFailureExistingVariable [

	model := self modelOnClasses: { RBLintRuleTestData }.

	self
		shouldFail: (self createRefactoringWithModel: model andArguments: {
						 {
							 (15 to: 19).
							 #rules }.
						 'name'.
						 #RBLintRuleTestData.
						 #openEditor });
		shouldFail: (self createRefactoringWithModel: model andArguments: {
						 {
							 (15 to: 19).
							 #rules }.
						 'rules'.
						 #RBLintRuleTestData.
						 #openEditor })
]

{ #category : 'failure tests' }
RBRenameTemporaryParametrizedTest >> testFailureModelBadName [

	| class |
	model := self modelOnClasses: { Object }.
	class := model classNamed: #Object.
	class
		compile:
		'aMethod: temp1 ^[| temp2 | temp2 := [:temp3 | temp3 = 5] value: 5. temp2] value'
		classified: #( #accessing ).
	self
		shouldFail: (self createRefactoringWithModel: model andArguments: {
						 {
							 (20 to: 24).
							 #temp2 }.
						 'temp3'.
						 #Object.
						 #aMethod: });
		shouldFail: (self createRefactoringWithModel: model andArguments: {
						 {
							 (20 to: 24).
							 #temp2 }.
						 'temp1'.
						 #Object.
						 #aMethod: })
]

{ #category : 'failure tests' }
RBRenameTemporaryParametrizedTest >> testFailureModelExistingVariable [

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

	self shouldFail: (self
			 createRefactoringWithModel: transformation model
			 andArguments: {
					 {
						 (16 to: 19).
						 #temp }.
					 #bar.
					 self class name.
					 #foo })
]

{ #category : 'tests' }
RBRenameTemporaryParametrizedTest >> testRenameArg [
	| refactoring |
	model := self modelOnClasses: { RBLintRuleTestData }.
	
	refactoring := self createRefactoringWithModel: model andArguments: { { (7 to: 13) . #aString } .
		'asdf' . #RBLintRuleTestData . #name: }.
	self executeRefactoring: refactoring.

	self assert: ((refactoring model classNamed: #RBLintRuleTestData) parseTreeForSelector: #name:) equals: (self parseMethod: 'name: asdf
	name := asdf')
]

{ #category : 'tests' }
RBRenameTemporaryParametrizedTest >> testRenameTemporary [
	| refactoring |
	model := self modelOnClasses: { RBLintRuleTestData }.
	
	refactoring := self createRefactoringWithModel: model andArguments: { {(15 to: 19) . #rules} .
		'asdf' . #RBLintRuleTestData .#openEditor }.
	self executeRefactoring: refactoring.

	self assert: ((refactoring model classNamed: #RBLintRuleTestData) parseTreeForSelector: #openEditor) equals: (self parseMethod: 'openEditor
								| asdf |
								asdf := self failedRules.
								asdf isEmpty ifTrue: [^self].
								asdf size == 1 ifTrue: [^asdf first viewResults]')
]
