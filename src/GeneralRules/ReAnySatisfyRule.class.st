"
Checks for the common code fragment: ""(aCollection detect: [:each | ''some condition''] ifNone: [nil]) ~= nil"". contains: can simplify this code to ""aCollection anySatisfy: [:each | ''some condition'']"". Not only is the anySatisfy: variant shorter, it better signifies what the code is doing.
"
Class {
	#name : #ReAnySatisfyRule,
	#superclass : #ReNodeMatchRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
ReAnySatisfyRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'AnySatisfyRule'
]

{ #category : #accessing }
ReAnySatisfyRule >> group [
	^ 'Optimization'
]

{ #category : #initialization }
ReAnySatisfyRule >> initialize [
	super initialize.
	self matchesAny: #(
			'(`@object detect: [:`each | | `@temps| `@.Statements] ifNone: [nil]) isNil'
			'(`@object detect: [:`each | | `@temps| `@.Statements] ifNone: [nil]) notNil'
			'(`@object detect: [:`each | | `@temps| `@.Statements] ifNone: [nil]) = nil'
			'(`@object detect: [:`each | | `@temps| `@.Statements] ifNone: [nil]) == nil'
			'(`@object detect: [:`each | | `@temps| `@.Statements] ifNone: [nil]) ~= nil'
			'(`@object detect: [:`each | | `@temps| `@.Statements] ifNone: [nil]) ~~ nil'
			'`@object detect: [:`each | | `@temps| `@.Statements] ifNone: [| `@temps1 | `@.Statements2. ^`@anything]' )
]

{ #category : #accessing }
ReAnySatisfyRule >> name [
	^ 'Uses detect:ifNone: instead of anySatisfy:'
]

{ #category : #accessing }
ReAnySatisfyRule >> rationale [
	^ 'Checks for the common code fragment: "(aCollection detect: [:each | ''some condition''] ifNone: [nil]) ~= nil".'
]
