Class {
	#name : 'ReGenerateEqualAndHashDriverTest',
	#superclass : 'ReDriverTest',
	#instVars : [
		'testingEnvironment'
	],
	#category : 'Refactoring-UI-Tests-Driver',
	#package : 'Refactoring-UI-Tests',
	#tag : 'Driver'
}

{ #category : 'tests' }
ReGenerateEqualAndHashDriverTest >> classForEqualAndHash [
	"Answer the <Class> which will be used to generate equal and hash methods"
	
	^ ReClassForGeneratingEqualAndHash 
]

{ #category : 'tests' }
ReGenerateEqualAndHashDriverTest >> classForEqualAndHashExistingImplementors [
	"Answer the <Class> which will be used to generate equal and hash methods"
	
	^ ReClassForGeneratingEqualAndHashExistingImplementors
]

{ #category : 'tests' }
ReGenerateEqualAndHashDriverTest >> testGenerateEqualAndHashExistingImplementors [

	| driver rbClass driverChanges |

	testingEnvironment := RBClassEnvironment class: self classForEqualAndHashExistingImplementors. 	
	driver := ReGenerateEqualAndHashDriver new. 
	self setUpDriver: driver.
	driver scopes: { testingEnvironment }.
	rbClass := testingEnvironment class.

	self assert: (driver targetClass definesMethod: #hash).
	self assert: (driver targetClass definesMethod: #=).

	SpWindowSimulateOpenModal 
		value: [ :aDiag | 
				aDiag presenter selectedItems: #(#name #iVarB). 
				aDiag triggerOkAction ]
		during: [ driver runRefactoring ].
	driverChanges := driver refactoring changes.

	self 
		assert: driverChanges changes size 
		equals: 2.
	self assertEmpty: driver refactoring failedApplicabilityPreconditions.
	self 
		assert: (driverChanges changes select: [ : amc | amc selector = #hash or: [ amc selector = #= ] ]) size
		equals: 2.

]

{ #category : 'tests' }
ReGenerateEqualAndHashDriverTest >> testGenerateEqualAndHashFirstGen [

	| driver rbClass driverChanges |

	testingEnvironment := RBClassEnvironment class: self classForEqualAndHash.
	driver := ReGenerateEqualAndHashDriver new.
	self setUpDriver: driver.
	driver scopes: { testingEnvironment }.
	rbClass := testingEnvironment class.

	self deny: (driver targetClass definesMethod: #hash).
	
	SpWindowSimulateOpenModal 
		value: [ :aDiag | 
				aDiag presenter selectedItems: #(#iVar2 #iVar1). 
				aDiag triggerOkAction ]
		during: [ driver runRefactoring ].
	driverChanges := driver refactoring changes.
	self 
		assert: driverChanges changes size 
		equals: 2.
	self assertEmpty: driver refactoring failedApplicabilityPreconditions.
	self 
		assert: (driverChanges changes select: [ : amc | amc selector = #hash or: [ amc selector = #= ] ]) size
		equals: 2.

	testingEnvironment ifNotNil: [ 
		testingEnvironment classesDo: [ : cls |
			(cls includesSelector: #=)
				ifTrue: [ cls removeSelector: #= ].
			(cls includesSelector: #hash)
				ifTrue: [ cls removeSelector: #hash ] ] ].

]
