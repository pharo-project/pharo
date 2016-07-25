testing
testAllGlobalBindingAreGlobalVariables
	| wrong |
	wrong := Smalltalk globals associations reject: [ :each | each isKindOf:  GlobalVariable].
	self assert: wrong isEmpty.