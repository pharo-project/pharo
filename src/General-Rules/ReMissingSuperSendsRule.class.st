"
Checks that methods who should always contain a super message send, actually contain a super message send. For example, the postCopy method should always contain a ""super postCopy"". 

The list of methods that should contain super message sends is in #superMessages.
"
Class {
	#name : 'ReMissingSuperSendsRule',
	#superclass : 'ReAbstractRule',
	#category : 'General-Rules-Potential Bugs',
	#package : 'General-Rules',
	#tag : 'Potential Bugs'
}

{ #category : 'testing' }
ReMissingSuperSendsRule class >> checksMethod [
	^ true
]

{ #category : 'accessing' }
ReMissingSuperSendsRule class >> group [
	^ self potentialBugsGroup
]

{ #category : 'accessing' }
ReMissingSuperSendsRule class >> ruleName [
	^ 'Missing super sends in selected methods.'
]

{ #category : 'accessing' }
ReMissingSuperSendsRule class >> severity [

	^ #error
]

{ #category : 'accessing' }
ReMissingSuperSendsRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'MissingSuperSendsRule'
]

{ #category : 'running' }
ReMissingSuperSendsRule >> basicCheck: aMethod [

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

{ #category : 'accessing' }
ReMissingSuperSendsRule >> methodsRequiringSuper [

	^ #( #release #postCopy #postBuildWith: #preBuildWith: #postOpenWith:
	     #noticeOfWindowClose: #initialize #initializeWindow: ) 
]
