"
Checks that methods that should always contain a super message send, actually contain a super message send. For example, the postCopy method should always contain a ""super postCopy"". The list of methods that should contain super message sends is in #superMessages.
"
Class {
	#name : #RBMissingSuperSendsRule,
	#superclass : #ReAbstractRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #testing }
RBMissingSuperSendsRule class >> checksMethod [
	^ true
]

{ #category : #accessing }
RBMissingSuperSendsRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'MissingSuperSendsRule'
]

{ #category : #running }
RBMissingSuperSendsRule >> check: aMethod forCritiquesDo: aCriticBlock [
	| definer superMethod |
	aMethod methodClass isMeta ifTrue: [ ^ self ].
	(self methodsRequiringSuper includes: aMethod selector) ifFalse: [ ^ self ]. 
	
	definer := aMethod methodClass superclass ifNotNil: [ :sc |
		sc whichClassIncludesSelector: aMethod selector ].
	definer ifNil: [ ^ self ].
	
	"super defines same method"
	(aMethod superMessages includes: aMethod selector) ifTrue: [ ^self ]. 
	
	"but I don't call it"
	superMethod := definer 
		compiledMethodAt: aMethod selector
		ifAbsent: [  ].
	
	superMethod isReturnSelf ifTrue: [ ^ self ].
	(superMethod sendsSelector: #subclassResponsibility) ifTrue: [ ^ self ].
	
	aCriticBlock cull: (self critiqueFor: aMethod)
]

{ #category : #accessing }
RBMissingSuperSendsRule >> group [
	^ 'Potential Bugs'
]

{ #category : #'as yet unclassified' }
RBMissingSuperSendsRule >> methodsRequiringSuper [
	^#(#release #postCopy #postBuildWith: #preBuildWith: #postOpenWith: #noticeOfWindowClose: #initialize)
]

{ #category : #accessing }
RBMissingSuperSendsRule >> name [
	^ 'Missing super sends in selected methods.'
]
