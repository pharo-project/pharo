"
Check for code that checks that a collection is non-empty before sending it an iteration message (e.g., do:, collect:, etc.). Since the collection iteration messages work for empty collections, we do not need to clutter up our method with the extra size check.
"
Class {
	#name : #RBSizeCheckRule,
	#superclass : #ReNodeMatchRule,
	#category : #GeneralRules-Migrated
}

{ #category : #accessing }
RBSizeCheckRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'SizeCheckRule'
]

{ #category : #private }
RBSizeCheckRule >> genericPatternForSelector: aSymbol [
	^ String streamContents: [ :stream |
		aSymbol keywords keysAndValuesDo: [ :index :value |
			stream space; nextPutAll: value.
			aSymbol last = $:
				ifTrue: [ stream space; nextPutAll: '`@object'; print: index ] ] ]
]

{ #category : #accessing }
RBSizeCheckRule >> group [
	^ 'Optimization'
]

{ #category : #initialization }
RBSizeCheckRule >> initialize [
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
RBSizeCheckRule >> name [
	^ 'Unnecessary size check'
]

{ #category : #private }
RBSizeCheckRule >> selectors [
	^ #( collect: do: reject: select: )
]
