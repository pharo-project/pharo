"
Check for code that checks that a collection is non-empty before sending it an iteration message (e.g., do:, collect:, etc.). Since the collection iteration messages work for empty collections, we do not need to clutter up our method with the extra size check.
"
Class {
	#name : #ReSizeCheckRule,
	#superclass : #ReNodeMatchRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
ReSizeCheckRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'SizeCheckRule'
]

{ #category : #private }
ReSizeCheckRule >> genericPatternForSelector: aSymbol [
	^ String streamContents: [ :stream |
		aSymbol keywords keysAndValuesDo: [ :index :value |
			stream space; nextPutAll: value.
			aSymbol last = $:
				ifTrue: [ stream space; nextPutAll: '`@object'; print: index ] ] ]
]

{ #category : #accessing }
ReSizeCheckRule >> group [
	^ 'Optimization'
]

{ #category : #initialization }
ReSizeCheckRule >> initialize [
	| patterns |
	super initialize.
	patterns := OrderedCollection new.
	patterns addAll: (self selectors
		collect: [ :each | '`@object size > 0 ifTrue: [`@object' , (self genericPatternForSelector: each) , '. `@.Statements2]' ]).
	patterns addAll: (self selectors
		collect: [ :each | '`@object isEmpty ifFalse: [`@object' , (self genericPatternForSelector: each) , '. `@.Statements2]' ]).
	patterns addAll: (self selectors
		collect: [ :each | '`@object notEmpty ifTrue: [`@object' , (self genericPatternForSelector: each) , '. `@.Statements2]' ]).
	patterns addAll: (self selectors
		collect: [ :each | '`@object size = 0 ifFalse: [`@object' , (self genericPatternForSelector: each) , '. `@.Statements2]' ]).
	self matchesAny: patterns
		
]

{ #category : #accessing }
ReSizeCheckRule >> name [
	^ 'Unnecessary size check'
]

{ #category : #private }
ReSizeCheckRule >> selectors [
	^ #( collect: do: reject: select: )
]
