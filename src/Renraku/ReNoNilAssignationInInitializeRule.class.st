"
Instance variables are initialized to nil by default. There is no need to do it explicitly in the initialize method.
"
Class {
	#name : #ReNoNilAssignationInInitializeRule,
	#superclass : #ReAbstractRule,
	#category : #'Renraku-Rules'
}

{ #category : #'as yet unclassified' }
ReNoNilAssignationInInitializeRule class >> checksMethod [
	^ true
]

{ #category : #running }
ReNoNilAssignationInInitializeRule >> basicCheck: aMethod [
	(aMethod selector = #initialize and: [ aMethod methodClass isInstanceSide ]) ifFalse: [ ^ false ].

	aMethod ast assignmentNodes detect: [ :assignment | assignment value value isNil ] ifFound: [ ^ true ].

	^ false
]

{ #category : #accessing }
ReNoNilAssignationInInitializeRule >> group [

	^ 'Style'
]

{ #category : #accessing }
ReNoNilAssignationInInitializeRule >> name [

	^ 'Initialize method does not need nil assignation'
]

{ #category : #accessing }
ReNoNilAssignationInInitializeRule >> severity [
	^ #information
]
