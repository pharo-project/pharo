"
Nobody should directly access the method dictionary. It is purely an implementation artefact that we use one dictionary and it might change in the future.
"
Class {
	#name : #RBSendsMethodDictRule,
	#superclass : #ReAbstractRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #testing }
RBSendsMethodDictRule class >> checksMethod [
	^ true
]

{ #category : #accessing }
RBSendsMethodDictRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'NobodyShouldSendMethodDict'
]

{ #category : #running }
RBSendsMethodDictRule >> basicCheck: aMethod [
	({Behavior . ClassDescription . Class} includes: aMethod methodClass) ifTrue: [ ^ false ].
	^ aMethod messages includes: #methodDict
]

{ #category : #accessing }
RBSendsMethodDictRule >> group [
	^ 'Bugs'
]

{ #category : #accessing }
RBSendsMethodDictRule >> name [
	^ 'No direct access of methodDict'
]

{ #category : #accessing }
RBSendsMethodDictRule >> rationale [
	^ 'nobody should directly access the method dictionary. It is purely an implementation artefact that we use one dictionary and it might change in the future'
]

{ #category : #accessing }
RBSendsMethodDictRule >> severity [
	^ #error
]
