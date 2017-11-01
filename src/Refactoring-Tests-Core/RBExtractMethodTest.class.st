Class {
	#name : #RBExtractMethodTest,
	#superclass : #RBRefactoringTest,
	#category : 'Refactoring-Tests-Core'
}

{ #category : #'failure tests' }
RBExtractMethodTest >> testBadInterval [
	self
		shouldFail: (RBExtractMethodRefactoring 
				extract: (self 
						convertInterval: (24 to: 30)
						for: (RBRefactoryTestDataApp sourceCodeAt: #testMethod))
				from: #testMethod
				in: RBRefactoryTestDataApp);
		shouldFail: (RBExtractMethodRefactoring 
				extract: (self 
						convertInterval: (80 to: 147)
						for: (RBBasicLintRuleTest class sourceCodeAt: #subclassOf:overrides:))
				from: #subclassOf:overrides:
				in: RBBasicLintRuleTest class)
]

{ #category : #'failure tests' }
RBExtractMethodTest >> testExtractFailure [
	self
		shouldFail: (RBExtractMethodRefactoring 
				extract: (self 
						convertInterval: (80 to: 269)
						for: (RBBasicLintRuleTest class sourceCodeAt: #subclassOf:overrides:))
				from: #subclassOf:overrides:
				in: RBBasicLintRuleTest class);
		shouldFail: (RBExtractMethodRefactoring 
				extract: (self 
						convertInterval: (53 to: 56)
						for: (RBBasicLintRuleTest class sourceCodeAt: #subclassOf:overrides:))
				from: #subclassOf:overrides:
				in: RBBasicLintRuleTest class);
		shouldFail: (RBExtractMethodRefactoring 
				extract: (self 
						convertInterval: (77 to: 222)
						for: (RBBasicLintRuleTest class sourceCodeAt: #subclassResponsibilityNotDefined))
				from: #subclassResponsibilityNotDefined
				in: RBBasicLintRuleTest class)
]

{ #category : #tests }
RBExtractMethodTest >> testExtractMethodAtEndOfMethodThatNeedsReturn [
	| refactoring class |
	refactoring := RBExtractMethodRefactoring 
		extract: (self 
				convertInterval: (52 to: 133)
				for: (RBLintRuleTest sourceCodeAt: #openEditor))
		from: #openEditor
		in: RBLintRuleTest.
	self 
		setupMethodNameFor: refactoring
		toReturn: #foo:.
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBLintRuleTest.
	self assert: (class parseTreeFor: #openEditor) = (RBParser parseMethod: 'openEditor
	| rules |
	rules := self failedRules.
	^self foo: rules').
	self assert: (class parseTreeFor: #foo:) = (RBParser parseMethod: 'foo: rules
	rules isEmpty ifTrue: [^self].
	rules size == 1 ifTrue: [^rules first viewResults]')
]

{ #category : #tests }
RBExtractMethodTest >> testExtractMethodThatMovesTemporaryVariable [
	| refactoring class |
	refactoring := RBExtractMethodRefactoring 
		extract: (self 
				convertInterval: (22 to: 280)
				for: (RBTransformationRuleTest sourceCodeAt: #superSends))
		from: #superSends
		in: RBTransformationRuleTest.
	self 
		setupMethodNameFor: refactoring
		toReturn: #foo1.
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBTransformationRuleTest.
	self assert: (class parseTreeFor: #superSends) = (RBParser parseMethod: 'superSends
	| rule |
	rule := self foo1.
	self rewriteUsing: rule').
	self assert: (class parseTreeFor: #foo1) = (RBParser parseMethod: 'foo1 | rule | 	rule := RBParseTreeRewriter new.
	rule addSearch: ''super `@message: ``@args''
				-> (
					[:aNode | 
					(class withAllSubclasses
						detect: [:each | each includesSelector: aNode selector]
						ifNone: [nil]) isNil]
							-> ''self `@message: ``@args'').
		^rule')
]

{ #category : #tests }
RBExtractMethodTest >> testExtractMethodThatNeedsArgument [
	| refactoring class |
	refactoring := RBExtractMethodRefactoring 
		extract: (self 
				convertInterval: (145 to: 343)
				for: (RBTransformationRuleTest sourceCodeAt: #checkMethod:))
		from: #checkMethod:
		in: RBTransformationRuleTest.
	self 
		setupMethodNameFor: refactoring
		toReturn: #foo:.
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBTransformationRuleTest.
	self assert: (class parseTreeFor: #checkMethod:) = (RBParser parseMethod: 'checkMethod: aSmalllintContext 
	class := aSmalllintContext selectedClass.
	(rewriteRule executeTree: aSmalllintContext parseTree) ifTrue: 
			[self foo: aSmalllintContext]').
	self assert: (class parseTreeFor: #foo:) = (RBParser parseMethod: 'foo: aSmalllintContext (RecursiveSelfRule executeTree: rewriteRule tree initialAnswer: false)
				ifFalse: 
					[builder compile: rewriteRule tree printString
						in: class
						classified: aSmalllintContext protocols]')
]

{ #category : #tests }
RBExtractMethodTest >> testExtractMethodThatNeedsTemporaryVariable [
	| refactoring class |
	refactoring := RBExtractMethodRefactoring 
		extract: (self 
				convertInterval: (78 to: 197)
				for: (RBLintRuleTest sourceCodeAt: #displayName))
		from: #displayName
		in: RBLintRuleTest.
	self 
		setupMethodNameFor: refactoring
		toReturn: #foo:.
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBLintRuleTest.
	self assert: (class parseTreeFor: #displayName) = (RBParser parseMethod: 'displayName
	| nameStream |
	nameStream := WriteStream on: (String new: 64).
	self foo: nameStream.
	^nameStream contents').
	self assert: (class parseTreeFor: #foo:) = (RBParser parseMethod: 'foo: nameStream 	nameStream nextPutAll: self name;
		nextPutAll: '' (''.
	self problemCount printOn: nameStream.
	nameStream nextPut: $).')
]

{ #category : #tests }
RBExtractMethodTest >> testModelExtractMethodWithTemporariesSelected [
	| class refactoring |
	model := RBClassModelFactory rbNamespace new.
	class := model classNamed: self class name.
	class compile: 'foo [| temp | temp := 5. temp * temp] value'
		classified: #(#accessing).
	refactoring := RBExtractMethodRefactoring 
				model: model
				extract: (6 to: 36)
				from: #foo
				in: class.
	self setupMethodNameFor: refactoring toReturn: #foobar.
	self executeRefactoring: refactoring.
	self assert: (class parseTreeFor: #foo) 
				= (RBParser parseMethod: 'foo [self foobar] value').
	self assert: (class parseTreeFor: #foobar) 
				= (RBParser parseMethod: 'foobar |temp | temp := 5. ^temp * temp')
]

{ #category : #tests }
RBExtractMethodTest >> testModelExtractMethodWithTemporaryAssigned [
	| class refactoring |
	model := RBClassModelFactory rbNamespace new.
	class := model classNamed: self class name.
	class 
		compile: 'foo 				| temp bar | 				bar := 5. 				temp := bar * bar. 				Transcript show: temp printString; cr. 				^temp * temp'
		classified: #(#accessing).
	refactoring := RBExtractMethodRefactoring 
				model: model
				extract: (26 to: 102)
				from: #foo
				in: class.
	self setupMethodNameFor: refactoring toReturn: #foobar.
	self executeRefactoring: refactoring.
	self assert: (class parseTreeFor: #foo) 
				= (RBParser parseMethod: 'foo | temp | temp := self foobar. ^temp * temp').
	self 
		assert: ((class parseTreeFor: #foobar) = (RBParser 
						parseMethod: 'foobar | bar temp | bar := 5. temp := bar * bar. Transcript show: temp printString; cr. ^temp.')) |
				((class parseTreeFor: #foobar) = (RBParser 
						parseMethod: 'foobar | temp bar | bar := 5. temp := bar * bar. Transcript show: temp printString; cr. ^temp.'))
]

{ #category : #'failure tests' }
RBExtractMethodTest >> testNonExistantSelector [
	self shouldFail: (RBExtractMethodRefactoring 
			extract: (10 to: 20)
			from: #checkClass1:
			in: RBBasicLintRuleTest)
]
