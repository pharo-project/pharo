testing
testAllClassPoolBindingAreClassVariables
	| wrong |
	wrong := OrderedCollection new.
	Smalltalk globals allClasses do: [:class | 
		 wrong addAll: (class classVariables reject: [ :each | each isKindOf:  ClassVariable]).
		].
	self assert: wrong isEmpty.