Class {
	#name : #DTCoveragePluginPresenterTest,
	#superclass : #TestCase,
	#instVars : [
		'presenter'
	],
	#category : #'DrTests-Tests'
}

{ #category : #running }
DTCoveragePluginPresenterTest >> setUp [

	super setUp.
	presenter := MockDTCoveragePluginPresenter new
]

{ #category : #tests }
DTCoveragePluginPresenterTest >> testEmptyInitializeOfAttributes [

	| emptyPresenter |
   emptyPresenter := MockDTCoveragePluginPresenter new .
	self assert: emptyPresenter uncoveredNodes isNil.
	self assert: emptyPresenter coveredNodes isNil.
   self assert: emptyPresenter yellowNodes isNil.
]

{ #category : #tests }
DTCoveragePluginPresenterTest >> testUpdateSourceCodePanelUncovered [

	| packageName package classes oneClass oneMethod  |
	packageName := 'DrTests-TestCoverage-Tests-Mocks'.
	package := RPackage allInstances
		           detect: [ :each | each name = packageName ]
		           ifNone: [ nil ].
	classes := package classes .
	oneClass := classes detect: [ :each | each name = 'MockForCoverage' ] ifNone: [ nil ].
	oneMethod := oneClass methodDict at: #method2ForMock: .
	presenter cleanAttributesForHighlighting .
	presenter defineColorCoverage: oneMethod.

	self assert: presenter uncoveredNodes size equals: 1.
	self assert: presenter coveredNodes size isZero .
   self assert: presenter yellowNodes size isZero.
]
