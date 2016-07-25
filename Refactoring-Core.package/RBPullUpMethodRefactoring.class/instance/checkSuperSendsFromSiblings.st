preconditions
checkSuperSendsFromSiblings
	| siblings |
	siblings := class superclass subclasses reject: [:each | each = class].
	siblings do: [:aRBClass | self checkSiblingSuperSendsFrom: aRBClass]