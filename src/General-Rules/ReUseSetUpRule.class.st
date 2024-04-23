"
this smell arise when we use initialize instead of setUp in a TestCase
"
Class {
	#name : 'ReUseSetUpRule',
	#superclass : 'ReAbstractRule',
	#category : 'General-Rules-Migrated',
	#package : 'General-Rules',
	#tag : 'Migrated'
}

{ #category : 'testing - interest' }
ReUseSetUpRule class >> checksClass [ 
	^ true
]

{ #category : 'manifest' }
ReUseSetUpRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^ 'UseSetUpRule'
]

{ #category : 'running' }
ReUseSetUpRule >> basicCheck: aClass [
	
	aClass isTestCase ifFalse: [ ^ false ].
	^ aClass selectors includes: #initialize
]

{ #category : 'accessing' }
ReUseSetUpRule >> group [

	^ 'Design Flaws'
]

{ #category : 'testing' }
ReUseSetUpRule >> isRewriteRule [

	^ true
]

{ #category : 'accessing' }
ReUseSetUpRule >> name [

	^ 'Uses setUp instead of initialize for Test class.'
]

{ #category : 'accessing' }
ReUseSetUpRule >> severity [ 

	^ #error
]
