"
List methods that have one or more arguments and do no refer to self or an instance variable. These methods might be better defined in some other class or as class methods.
"
Class {
	#name : #RBUtilityMethodsRule,
	#superclass : #ReAbstractRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #testing }
RBUtilityMethodsRule class >> checksMethod [
	^ true
]

{ #category : #accessing }
RBUtilityMethodsRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'UtilityMethodsRule'
]

{ #category : #running }
RBUtilityMethodsRule >> basicCheck: aMethod [
	^ (aMethod methodClass isMeta or: [ aMethod selector numArgs == 0 or: [ self utilityProtocols includes: aMethod protocol ] ]) not
		and: [ (self subclassOf: aMethod methodClass overrides: aMethod selector) not
				and: [ aMethod superMessages isEmpty
						and: [ aMethod selfMessages isEmpty and: [ aMethod methodClass allInstVarNames , aMethod methodClass allClassVarNames asArray , #('self') noneSatisfy: [ :each | aMethod ast references: each ] ] ] ] ]
]

{ #category : #accessing }
RBUtilityMethodsRule >> group [
	^ 'Design Flaws'
]

{ #category : #accessing }
RBUtilityMethodsRule >> name [
	^ 'Utility methods'
]

{ #category : #private }
RBUtilityMethodsRule >> subclassOf: aClass overrides: aSelector [ 
	^(aClass subclasses detect: 
			[:each | 
			(each includesSelector: aSelector)
				or: [self subclassOf: each overrides: aSelector]]
		ifNone: [nil]) notNil
]

{ #category : #private }
RBUtilityMethodsRule >> utilityProtocols [
	^ #('*utilit*')
]
