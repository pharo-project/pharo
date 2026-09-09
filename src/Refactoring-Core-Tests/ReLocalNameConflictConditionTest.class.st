"
A ReLocalNameConflictConditionTest is a test class for testing the behavior of ReLocalNameConflictCondition
"
Class {
	#name : 'ReLocalNameConflictConditionTest',
	#superclass : 'TestCase',
	#category : 'Refactoring-Core-Tests-Conditions',
	#package : 'Refactoring-Core-Tests',
	#tag : 'Conditions'
}

{ #category : 'tests' }
ReLocalNameConflictConditionTest >> testCheckDetectsNestedDefinitionConflict [
    | ast condition innerBlock |

    ast := RBParser parseMethod: '
    foo
		|temp|
		temp:= 3.
			
      [ | temp | temp := 1 ].

      ^temp
    '.

    innerBlock := ast body statements first.

    condition := ReLocalNameConflictCondition new
        variableName: 'temp'
        definingNode: innerBlock
        parseTree: ast.

    self deny: condition check.
]

{ #category : 'tests' }
ReLocalNameConflictConditionTest >> testCheckReturnsFalseWhenTemporaryAlreadyExists [

	| ast condition |

	ast := RBParser parseMethod: '
	foo
    | temp |
    temp := 1
	'.

	condition := ReLocalNameConflictCondition new
		variableName: 'temp'
		definingNode: ast body 
		parseTree: ast.
		
	self deny: condition check.
]

{ #category : 'tests' }
ReLocalNameConflictConditionTest >> testCheckReturnsTrueWhenVariableDoesNotExist [

	| ast condition |

	ast := RBParser parseMethod: '
	foo
		| a |
		a := 1.
		^a
	'.

	condition := ReLocalNameConflictCondition new
		variableName: 'newTemp'
		definingNode: ast body 
		parseTree: ast.

	self assert: condition check.
]

{ #category : 'tests' }
ReLocalNameConflictConditionTest >> testViolationMessageOn [

    | ast condition stream |

    ast := OCParser parseMethod:
	 'foo
   		| temp |
    	temp := 1'.

    condition := ReLocalNameConflictCondition new.

    condition
        variableName: 'temp'
        definingNode: ast body
        parseTree: ast.

    condition check.

    stream := String new writeStream.
    condition violationMessageOn: stream.

    self assert: stream contents equals:
        ('The variable name "temp" is already defined in the current scope.'
            , String cr).
]
