Class {
	#name : #RBAbstractEnvTestCase,
	#superclass : #ParametrizedTestCase,
	#instVars : [
		'testingEnvironment'
	],
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #utilities }
RBAbstractEnvTestCase class >> makeAllFailureShownInTestMethodNames [
	
	| methods nSel |
	methods := OrderedCollection new.
	RBAbstractEnvTestCase withAllSubclassesDo: [ :cls |

	methods addAll: (((cls methodsInProtocol: 'failure tests') 
		select: [ :each | 'test*' match: each selector  ] )
		reject: [ :each | 'testFailure*' match: each selector ]). ].


	methods do: [ :meth |
		nSel := 'testFailure', (meth selector allButFirst: 4) asSymbol.
	meth methodClass  
	compile: (meth ast selector: nSel) formattedCode 
	classified: 'failure tests']
]

{ #category : #utilities }
RBAbstractEnvTestCase class >> removeDuplicates [
	
	| methods  |
	methods := OrderedCollection new.
	RBAbstractEnvTestCase withAllSubclassesDo: [ :cls |

	methods addAll: (((cls methodsInProtocol: 'failure tests') 
		select: [ :each | 'test*' match: each selector  ] )
		reject: [ :each | 'testFailure*' match: each selector ]). ].
	methods do: [ :each | 
			each methodClass removeSelector:  each selector
			 ]

	
]

{ #category : #accessing }
RBAbstractEnvTestCase >> environmentOfTest [
	^ testingEnvironment
]

{ #category : #running }
RBAbstractEnvTestCase >> setUp [
	super setUp.
	testingEnvironment := Smalltalk globals
]

{ #category : #accessing }
RBAbstractEnvTestCase >> testingEnvironment: anObject [
	testingEnvironment := anObject
]
