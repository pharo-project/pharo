tests
testAsMutator

	self assert: #x asMutator = #x:.
	"return receiver if it is already a mutator"
	self assert: #x: asMutator = #x:.
	self assert: #x asMutator isSymbol
	