"
I'm a wrapper of renraku rules, so they can be used in other tools that follow the RB architecture
"
Class {
	#name : #RBRenrakuWrapperLintRule,
	#superclass : #RBBasicLintRule,
	#instVars : [
		'renrakuRule'
	],
	#category : #'Refactoring-Critics'
}

{ #category : #'instance creation' }
RBRenrakuWrapperLintRule class >> new [

	^ Error signal: 'Please use #new: and provide a renraku rule instance'
]

{ #category : #'instance creation' }
RBRenrakuWrapperLintRule class >> new: aRule [

	| annotatedClass |
	
	annotatedClass := self newAnonymousSubclass.
	annotatedClass class compile:
		'uniqueIdentifierName ^ ',
		aRule class uniqueIdentifierName surroundedBySingleQuotes.

	^ annotatedClass basicNew
		initialize: aRule;
		yourself
]

{ #category : #accessing }
RBRenrakuWrapperLintRule class >> uniqueIdentifierName [
	^ 'WrapperRule'
]

{ #category : #adding }
RBRenrakuWrapperLintRule >> addSearchStringFrom: aCritique in: aSourceCode [

	aCritique sourceAnchor providesInterval ifTrue: [
		| interval |
		interval := aCritique sourceAnchor interval.
		result addSearchString: (
			aSourceCode
				copyFrom: interval first
				to: interval last)  ].
]

{ #category : #running }
RBRenrakuWrapperLintRule >> byNodeCheckMethod: aMethod [

	aMethod ast nodesDo: [ :node |
		renrakuRule
			check: node
			forCritiquesDo: [ :crit |
				result addMethod: aMethod.
				self
					addSearchStringFrom: crit
					in: aMethod definition.
				^ self ] ]
	
]

{ #category : #running }
RBRenrakuWrapperLintRule >> checkClass: aClass [
	renrakuRule class checksClass ifFalse: [ ^ self ].
	renrakuRule
		check: aClass
		forCritiquesDo: [ :crit |
			result addClass: aClass.
			self
				addSearchStringFrom: crit
				in: aClass definition.
			^ self ]
]

{ #category : #running }
RBRenrakuWrapperLintRule >> checkMethod: aMethod [

	renrakuRule class checksNode ifTrue: [ 
		^ self byNodeCheckMethod: aMethod ].
	renrakuRule class checksMethod ifFalse: [ ^ self ].
	renrakuRule
		check: aMethod
		forCritiquesDo: [ :crit |
			result addMethod: aMethod.
			self
				addSearchStringFrom: crit
				in: aMethod definition.
			^ self ]
	
]

{ #category : #running }
RBRenrakuWrapperLintRule >> checkPackage: aPackage [
	renrakuRule class checksPackage ifFalse: [ ^ self ].
	renrakuRule
		check: aPackage
		forCritiquesDo: [ :crit |
			^ result addPackage: aPackage ]
]

{ #category : #accessing }
RBRenrakuWrapperLintRule >> group [

	^ renrakuRule group
]

{ #category : #initialization }
RBRenrakuWrapperLintRule >> initialize: aRule [
	renrakuRule := aRule.
	self initialize.
]

{ #category : #accessing }
RBRenrakuWrapperLintRule >> name [

	^ renrakuRule name
]

{ #category : #accessing }
RBRenrakuWrapperLintRule >> rationale [

	^ renrakuRule rationale
]

{ #category : #accessing }
RBRenrakuWrapperLintRule >> resultClass [

	renrakuRule class checksNode    ifTrue: [ ^ RBSelectorEnvironment ].
	renrakuRule class checksMethod  ifTrue: [ ^ RBSelectorEnvironment ].
	renrakuRule class checksClass   ifTrue: [ ^ RBClassEnvironment ].
	renrakuRule class checksPackage ifTrue: [ ^ RBPackageEnvironment ].
	^ RBMultiEnvironment
]
