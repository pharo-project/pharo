query
methodsWithUnboundGlobals
	"This says that for any global, it should match either the class's notion of what bindingOf: the key is, or bindingOf: should be nil and the binding should be in Undeclared. If the class answers a different binding through bindingOf: or answers no binding and the binding is not in Undeclared then the variable in the method is wrong.
	For a clean image the result should be empty. If it is not empty, evaluating Compiler recompileAll probably solves the problem. However, we should investigate why the method gets an incorrect state."

	"SystemNavigation new methodsWithUnboundGlobals"

	^self allMethodsSelect:
		[:m|
		m literals anySatisfy:
			[:l|
			l isVariableBinding
			and: [l key isSymbol "avoid class-side methodClass literals"
			and: [ (l value isKindOf: Slot) not
			and: [(m methodClass classBindingOf: l key)
					ifNil: [(Undeclared associationAt: l key ifAbsent: []) ~~ l]
					ifNotNil: [:b| b ~~ l]]]]]]