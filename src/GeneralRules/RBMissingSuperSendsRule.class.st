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
RBMissingSuperSendsRule >> basicCheck: aMethod [
	| definer superMethod |
	aMethod methodClass isMeta ifTrue: [ ^ false ].
	(self methodsRequiringSuper includes: aMethod selector) ifFalse: [ ^ false ].

	definer := aMethod methodClass superclass ifNotNil: [ :sc | sc whichClassIncludesSelector: aMethod selector ].
	definer ifNil: [ ^ false ].

	"super defines same method"
	(aMethod superMessages includes: aMethod selector) ifTrue: [ ^ false ].

	"but I don't call it"
	superMethod := definer compiledMethodAt: aMethod selector ifAbsent: [  ].

	superMethod isReturnSelf ifTrue: [ ^ false ].
	^ (superMethod sendsSelector: #subclassResponsibility) not
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
