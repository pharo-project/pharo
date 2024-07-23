"
Instance variables are initialized to nil by default. There is no need to do it explicitly in the initialize method.
"
Class {
	#name : 'ReNoNilAssignationInInitializeRule',
	#superclass : 'ReAbstractRule',
	#category : 'General-Rules-Style',
	#package : 'General-Rules',
	#tag : 'Style'
}

{ #category : 'testing - interest' }
ReNoNilAssignationInInitializeRule class >> checksMethod [
	^ true
]

{ #category : 'accessing' }
ReNoNilAssignationInInitializeRule class >> group [

	^ 'Style'
]

{ #category : 'accessing' }
ReNoNilAssignationInInitializeRule class >> ruleName [
	^ 'Initialize method does not need nil assignation'
]

{ #category : 'accessing' }
ReNoNilAssignationInInitializeRule class >> severity [
	^ #information
]

{ #category : 'running' }
ReNoNilAssignationInInitializeRule >> check: aMethod forCritiquesDo: aCriticBlock [
	| problemAssigments|
	(aMethod selector = #initialize and: [ aMethod methodClass isInstanceSide ]) ifFalse: [ ^ self ].

	problemAssigments := aMethod ast assignmentNodes select: [ :assignment | assignment value value isNil ].

	problemAssigments do: [ :assignment |
			aCriticBlock cull: (self createTrivialCritiqueOn: aMethod intervalOf: assignment hint: 'nil').
		 ]
]
