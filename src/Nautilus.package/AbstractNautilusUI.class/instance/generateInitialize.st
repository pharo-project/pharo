menus behavior
generateInitialize
	
	self selectedClass ifNotNil: [:class || code protocol |
		protocol := 'initialization'.
		(class isMeta not and: [class isTestCase])
			ifTrue: [
				(class includesSelector:  #setUp) 
					ifFalse: [
						code := self buildSetUpCodeFor: class.
						class compile: code classified: protocol ].
				(class includesSelector: #tearDown) 
					ifFalse: [
						code := self buildTearDownCodeFor: class.
						class compile: code classified: protocol ].
				self selectedMethod: (class>>#setUp) 
				]
			ifFalse: [
				(class includesSelector: #initialize) 
					ifFalse: [
						code := self buildInitializeCodeFor: class.
						class compile: code classified: protocol ].
				self selectedMethod: (class>>#initialize) ].
		self updateCategoryAndMethodList.
		self giveFocusToSourceCodeArea ]