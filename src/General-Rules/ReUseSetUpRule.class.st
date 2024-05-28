"
this smell arise when we use initialize instead of setUp in a TestCase
"
Class {
	#name : 'ReUseSetUpRule',
	#superclass : 'ReNodeRewriteRule',
	#category : 'General-Rules-Migrated',
	#package : 'General-Rules',
	#tag : 'Migrated'
}

{ #category : 'accessing' }
ReUseSetUpRule class >> group [

	^ 'Design Flaws'
]

{ #category : 'manifest' }
ReUseSetUpRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^ 'UseSetUpRule'
]

{ #category : 'initialization' }
ReUseSetUpRule >> initialize [

	super initialize.
	self
		addMatchingMethod:
		'initialize |`@temps| super initialize. `.@statements'
		rewriteTo: 'setUp |`@temps| super setUp. `.@statements'.
	self addMatchingMethod: 'initialize' rewriteTo: 'setUp'
]

{ #category : 'accessing' }
ReUseSetUpRule >> name [

	^ 'Uses setUp instead of initialize for Test class.'
]

{ #category : 'accessing' }
ReUseSetUpRule >> severity [

	^ #error 
]

{ #category : 'testing' }
ReUseSetUpRule >> shouldCheckMethod: aMethod [ 

	^ aMethod selector = #initialize and: [ aMethod methodClass isTestCase ]
]
