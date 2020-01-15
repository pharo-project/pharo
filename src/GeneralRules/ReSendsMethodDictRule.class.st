"
Nobody should directly access the method dictionary. It is purely an implementation artefact that we use one dictionary and it might change in the future.
"
Class {
	#name : #ReSendsMethodDictRule,
	#superclass : #ReAbstractRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #testing }
ReSendsMethodDictRule class >> checksMethod [
	^ true
]

{ #category : #accessing }
ReSendsMethodDictRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'NobodyShouldSendMethodDict'
]

{ #category : #running }
ReSendsMethodDictRule >> basicCheck: aMethod [
	({Behavior . ClassDescription . Class} includes: aMethod methodClass) ifTrue: [ ^ false ].
	^ aMethod messages includes: #methodDict
]

{ #category : #accessing }
ReSendsMethodDictRule >> group [
	^ 'Bugs'
]

{ #category : #accessing }
ReSendsMethodDictRule >> name [
	^ 'No direct access of methodDict'
]

{ #category : #accessing }
ReSendsMethodDictRule >> rationale [
	^ 'nobody should directly access the method dictionary. It is purely an implementation artefact that we use one dictionary and it might change in the future'
]

{ #category : #accessing }
ReSendsMethodDictRule >> severity [
	^ #error
]
