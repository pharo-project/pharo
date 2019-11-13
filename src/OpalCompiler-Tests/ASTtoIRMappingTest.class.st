"
test related to AST -> IR mapping and back
"
Class {
	#name : #ASTtoIRMappingTest,
	#superclass : #TestCase,
	#category : #'OpalCompiler-Tests-Mapping'
}

{ #category : #tests }
ASTtoIRMappingTest >> testOffsetVectorTemp [
	| ast var |
	"Temps that are stored in a temp vector have a unique index in the vector"
	ast := Smalltalk compiler
		parse:
			'm04_two_temps_one_temp_in_remote_vector

| t1 t2 |

t1 := 1.
[ t2 := t1 +  1.0 ] value.

^t2'.

	ast doSemanticAnalysis.
	var := ast blockNodes first variableNodes second binding.
	
	self assert: var vectorOffset equals: 1
]
