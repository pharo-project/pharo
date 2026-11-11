"
A test baseline: TestDiamond requires TestDiamondLeft and TestDiamondRight, which both require TestToLock2, from two different repositories (two versions of it).
"
Class {
	#name : 'BaselineOfTestDiamond',
	#superclass : 'BaselineOf',
	#category : 'Metacello-Tests-Specs',
	#package : 'Metacello-Tests',
	#tag : 'Specs'
}

{ #category : 'baselines' }
BaselineOfTestDiamond >> baseline: spec [

	<baseline>
	spec for: #common do: [
			spec baseline: 'TestDiamondLeft' with: [ spec repository: 'http://left.com' ].
			spec baseline: 'TestDiamondRight' with: [ spec repository: 'http://right.com' ] ]
]
