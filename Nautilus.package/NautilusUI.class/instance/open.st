open
open
	| focus |
	super open.
	window takeKeyboardFocus.
	focus := self packageWidget.
	self selectedClass
		ifNotNil: [ acceptor := ClassOrMethodDefinitionAcceptor model: self.
			focus := self classWidget ].
	self selectedCategory
		ifNotNil:
			[ :cat | 
			categoryWidget selectProtocol: cat.
			self resetMethodsList.
			acceptor := MethodDefinitionAcceptor model: self.
			focus := categoryWidget ].
	self selectedMethod
		ifNotNil: [ :meth | 
			methodWidget selectMethod: meth.
			acceptor := MethodDefinitionAcceptor model: self.
			focus := methodWidget ].
	self selectedClass ifNotNil: [ self updateCategoryAndMethodList: self selectedMethod ].
	self giveFocusTo: focus.