Class {
	#name : 'DTCoveragePluginPresenterTest',
	#superclass : 'TestCase',
	#instVars : [
		'presenter'
	],
	#category : 'DrTests-Tests',
	#package : 'DrTests-Tests'
}

{ #category : 'running' }
DTCoveragePluginPresenterTest >> setUp [

	super setUp.
	presenter := MockDTCoveragePluginPresenter new
]

{ #category : 'tests' }
DTCoveragePluginPresenterTest >> testEmptyInitializeOfAttributes [

	| emptyPresenter |
	emptyPresenter := MockDTCoveragePluginPresenter new .
	self assert: emptyPresenter uncoveredNodes isNil.
	self assert: emptyPresenter coveredNodes isNil.
	self assert: emptyPresenter yellowNodes isNil.
]

{ #category : 'tests' }
DTCoveragePluginPresenterTest >> testUpdateSourceCodePanelUncovered [

	| oneMethod  |
	oneMethod := MockForCoverage >> #method2ForMock:.
	presenter cleanAttributesForHighlighting .
	presenter defineColorCoverage: oneMethod.

	self assert: presenter uncoveredNodes size equals: 1.
	self assert: presenter coveredNodes size isZero .
	self assert: presenter yellowNodes size isZero.
]
