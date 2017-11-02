"
Checks for bytecodePrimEqual  using the do: method instead of using the contains: or detect: methods.
"
Class {
	#name : #RBDetectContainsRule,
	#superclass : #RBParseTreeLintRule,
	#category : 'Refactoring-Critics-ParseTreeRules'
}

{ #category : #accessing }
RBDetectContainsRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'DetectContainsRule'
]

{ #category : #accessing }
RBDetectContainsRule >> group [
	^ 'Coding Idiom Violation'
]

{ #category : #initialization }
RBDetectContainsRule >> initialize [
	super initialize.
	self matcher 
		matchesAnyOf: #(
			'`@collection do: [:`each | | `@temps | 
				`@.Statements1. 
				`@condition ifFalse: [| `@BlockTemps | `@.BlockStatements1. ^`each]. 
				`@.Statements2]'
			'`@collection do: [:`each | | `@temps | 
				`@.Statements1. 
				`@condition ifTrue: [| `@BlockTemps | `@.BlockStatements1.  ^`each]. 
				`@.Statements2]'
			'`@collection do: [:`each | | `@temps | 
				`@.Statements1. 
				`@condition ifFalse: [| `@BlockTemps | `@.BlockStatements1. ^true]. 
				`@.Statements2]'
			'`@Collection do: [:`each | | `@temps | 
				`@.Statements1. 
				`@condition ifTrue: [| `@BlockTemps | `@.BlockStatements1.  ^true]. 
				`@.Statements2]'
			'`@collection do: [:`each | | `@temps | 
				`@.Statements1. 
				`@condition ifFalse: [| `@BlockTemps | `@.BlockStatements1. ^false]. 
				`@.Statements2]'
			'`@collection do: [:`each | | `@temps | 
				`@.Statements1. 
				`@condition ifTrue: [| `@BlockTemps | `@.BlockStatements1.  ^false]. 
				`@.Statements2]' )
		do: [ :node :answer | node ]
]

{ #category : #accessing }
RBDetectContainsRule >> name [
	^ 'Uses do: instead of contains: or detect:''s'
]
