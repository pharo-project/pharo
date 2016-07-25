menus behavior
forceGenerateInitialize

	self selectedClass ifNotNil: [:class || code |
		(class isMeta not and: [class isTestCase])
			ifTrue: [
				code := self buildSetUpCodeFor: class.
				class compile: code classified: 'initialization'.
				code := self buildTearDownCodeFor: class.
				class compile: code classified: 'initialization'.
				self selectedMethod: (class>>#setUp) ]
			ifFalse: [
				code := self buildInitializeCodeFor: class.
				class compile: code classified: 'initialization' .
				self selectedMethod: (class>>#initialize) ].
		self updateCategoryAndMethodList.
		self giveFocusToSourceCodeArea ]