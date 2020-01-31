"
Checks for bytecodePrimEqual  using the do: method instead of using the contains: or detect: methods.
"
Class {
	#name : #ReDetectContainsRule,
	#superclass : #ReNodeMatchRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
ReDetectContainsRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'DetectContainsRule'
]

{ #category : #accessing }
ReDetectContainsRule >> group [
	^ 'Coding Idiom Violation'
]

{ #category : #initialization }
ReDetectContainsRule >> initialize [
	super initialize.
	self matchesAny: #(
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
]

{ #category : #accessing }
ReDetectContainsRule >> name [
	^ 'Uses do: instead of contains: or detect:''s'
]
