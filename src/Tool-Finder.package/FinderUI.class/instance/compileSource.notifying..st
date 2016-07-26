text areas behavior
compileSource: aString notifying: aController

	| class method |
	(self selectedClass isNil or: [ self selectedMethod isNil ])
		ifTrue:[
			self changed: #clearUserEdits.
			^self].
	class := self selectedClass.
	method := class compiledMethodAt: self selectedMethod.
	(class compile: (aString asString) classified: method category notifying: aController)
			ifNil: [^ self ]
			ifNotNil: [:selector |
				self changed: #clearUserEdits ]