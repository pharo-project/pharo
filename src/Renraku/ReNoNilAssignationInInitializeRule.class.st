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
ReNoNilAssignationInInitializeRule >> check: aMethod forCritiquesDo: aCriticBlock [
	| problemAssigments|
	(aMethod selector = #initialize and: [ aMethod methodClass isInstanceSide ]) ifFalse: [ ^ self ].

	problemAssigments := aMethod ast assignmentNodes select: [ :assignment | assignment value value isNil ].
	
	problemAssigments do: [ :assignment | 
			aCriticBlock cull: (self createTrivialCritiqueOn: aMethod intervalOf: assignment hint: 'nil').
		 ]

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
