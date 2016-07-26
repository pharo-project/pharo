source code area
compileAMethodFromCategory: aCategory withSource: aString notifying: aController
	| category selector class  method |
	category := aCategory.
	class := self selectedClass ifNil: [ ^ Error signal: 'Should not happen!' ].
	(category = self noMethodsString or: [ category = self allLabel ])
		ifTrue: [ category := self asYetUnclassifiedString.
			self categoryWidget resetCategorySelection ].
	selector := self
		compileANewMethodInClass: class
		categorized: category
		from: aString
		notifyng: aController.
	selector ifNil: [ ^ self sourceTextModel hasUnacceptedEdits: true ].
	method := class methodNamed: selector.
	self sourceTextModel clearUserEdits.
	(self selectedMethod notNil and: [ self selectedClass = class and: [ self selectedMethod selector = selector ] ])
		ifFalse: [
			self selectedMethod: method.
			self updateCategoryAndMethodList	]