"
Replace ad-hoc implementations (using explicit logic based on do:) of ==allSatisfy:==, ==anySatisfy:== and ==noneSatisfy:== by the adequate calls to ==allSatisfy:==, ==anySatisfy:== or ==noneSatisfy:==. 

For example 

[[[  
collection do: [ :each |
				...
				condition
					ifFalse: [ ^ false ] ]
]]]
is transformed into 

[[[  
collection allSatisfy: [ :each | condition ]
]]]
"
Class {
	#name : #RBAllAnyNoneSatisfyRule,
	#superclass : #ReNodeRewriteRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
RBAllAnyNoneSatisfyRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'AllAnyNoneSatisfyRule'
]

{ #category : #accessing }
RBAllAnyNoneSatisfyRule >> group [
	^ 'Coding Idiom Violation'
]

{ #category : #initialization }
RBAllAnyNoneSatisfyRule >> initialize [
	super initialize.
	self
	
		" allSatisfy: "
		addMatchingMethod: '`@method: `@args
			| `@temps |
			`@.statements.
			`@collection do: [ :`each |
				| `@blocktemps |
				`@.blockstatements.
				`@condition
					ifFalse: [ ^ false ] ].
			^ true'
		rewriteTo: '`@method: `@args
			| `@temps |
			`@.statements. 
			^ `@collection allSatisfy: [ :`each |
				| `@blocktemps |
				`@.blockstatements.
				`@condition ]';

		" anySatisfy: "
		addMatchingMethod: '`@method: `@args
			| `@temps |
			`@.statements.
			`@collection do: [ :`each |
				| `@blocktemps |
				`@.blockstatements.
				`@condition
					ifTrue: [ ^ true ] ].
			^ false'
		rewriteTo: '`@method: `@args
			| `@temps |
			`@.statements.
			^ `@collection anySatisfy: [ :`each | 
				| `@blocktemps |
				`@.blockstatements.
				`@condition ]';

		" noneSatisfy: "
		addMatchingMethod: '`@method: `@args
			| `@temps |
			`@.statements.
			`@collection do: [ :`each |
				| `@blocktemps |
				`@.blockstatements.
				`@condition
					ifTrue: [ ^ false ] ].
			^ true'
		rewriteTo: '`@method: `@args
			| `@temps |
			`@.statements.
			^ `@collection noneSatisfy: [ :`each | 
				| `@blocktemps |
				`@.blockstatements.				
				`@condition ]'
]

{ #category : #accessing }
RBAllAnyNoneSatisfyRule >> name [
	^ 'Replace with #allSatisfy:, #anySatisfy: or #noneSatisfy:'
]

{ #category : #accessing }
RBAllAnyNoneSatisfyRule >> rationale [
	^ 'Replace ad-hoc implementations (using explicit logic based on do:) of allSatisfy:, anySatisfy: and noneSatisfy: by the adequate calls to #allSatisfy:, #anySatisfy: or #noneSatisfy:. '
]
