tests
testMoveMethodIntoArgument
	| refactoring class |
	self proceedThroughWarning: 
		[ refactoring := RBMoveMethodRefactoring 
			selector: #checkMethod:
			class: RBTransformationRuleTest
			variable: 'aSmalllintContext'.
		self 
			setupSelfArgumentNameFor: refactoring
			toReturn: 'transformationRule'.
		self 
			setupVariableTypesFor: refactoring
			toReturn: (Array with: (refactoring model classNamed: #RBSmalllintContext)).
		self 
			setupMethodNameFor: refactoring
			toReturn: #foo:.
		self executeRefactoring: refactoring ].
	class := refactoring model classNamed: #RBTransformationRuleTest.
	self assert: (class parseTreeFor: #checkMethod:) = (RBParser parseMethod: 'checkMethod: aSmalllintContext aSmalllintContext foo: self').
	self assert: ((refactoring model classNamed: #RBSmalllintContext) parseTreeFor: #foo:) = (RBParser parseMethod: 'foo: transformationRule
	transformationRule class: self selectedClass.
	(transformationRule rewriteRule executeTree: self parseTree) ifTrue: 
			[(transformationRule class recursiveSelfRule executeTree: transformationRule rewriteRule tree initialAnswer: false)
				ifFalse: 
					[transformationRule builder compile: transformationRule rewriteRule tree printString
						in: transformationRule class1
						classified: self protocols]]').
	self assert: (class parseTreeFor: #class1) = (RBParser parseMethod: 'class1 ^class').
	self assert: (class parseTreeFor: #class:) = (RBParser parseMethod: 'class: anObject class := anObject').
	self assert: (class theMetaClass parseTreeFor: #recursiveSelfRule:) = (RBParser parseMethod: 'recursiveSelfRule: anObject RecursiveSelfRule := anObject').
	self assert: (class theMetaClass parseTreeFor: #recursiveSelfRule) = (RBParser parseMethod: 'recursiveSelfRule ^RecursiveSelfRule').
	self assert: (class parseTreeFor: #builder) = (RBParser parseMethod: 'builder ^builder').
	self assert: (class parseTreeFor: #builder:) = (RBParser parseMethod: 'builder: anObject builder := anObject').
	self assert: (class parseTreeFor: #rewriteRule) = (RBParser parseMethod: 'rewriteRule ^rewriteRule').
	self assert: (class parseTreeFor: #rewriteRule:) = (RBParser parseMethod: 'rewriteRule: anObject rewriteRule := anObject')