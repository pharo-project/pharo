"
A test baseline: TestDiamond requires TestDiamondLeft and TestDiamondRight, which both require TestToLock2, from two different repositories (two versions of it).
"
Class {
	#name : 'BaselineOfTestDiamondLeft',
	#superclass : 'BaselineOf',
	#category : 'Metacello-Tests-Specs',
	#package : 'Metacello-Tests',
	#tag : 'Specs'
}

{ #category : 'baselines' }
BaselineOfTestDiamondLeft >> baseline: spec [

	<baseline>
	spec for: #common do: [ spec baseline: 'TestToLock2' with: [ spec repository: 'http://v1.com' ] ]
]
