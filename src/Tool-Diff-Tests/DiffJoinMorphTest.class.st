"
I check what a DiffJoinMorph tells its owner when a join is clicked.
"
Class {
	#name : 'DiffJoinMorphTest',
	#superclass : 'TestCase',
	#instVars : [
		'join'
	],
	#category : 'Tool-Diff-Tests',
	#package : 'Tool-Diff-Tests'
}

{ #category : 'helpers' }
DiffJoinMorphTest >> click [

	join mouseDown: (MouseButtonEvent new
		 setType: #mouseDown
		 position: 10 @ 50
		 which: MouseButtonEvent redButton
		 buttons: MouseButtonEvent redButton
		 hand: nil
		 stamp: 0)
]

{ #category : 'running' }
DiffJoinMorphTest >> setUp [

	super setUp.
	join := DiffJoinMorph new.
	join bounds: (0 @ 0 extent: 20 @ 100)
]

{ #category : 'tests' }
DiffJoinMorphTest >> testClickOnAJoinRunsTheBlock [

	| clicked |
	clicked := false.
	join mappings add: DiffJoinSectionStub new.
	join whenClickedDo: [ clicked := true ].

	self click.

	self assert: clicked
]

{ #category : 'tests' }
DiffJoinMorphTest >> testClickOutsideJoinsDoesNotRunTheBlock [

	| clicked |
	clicked := false.
	join whenClickedDo: [ clicked := true ].

	self click.

	self deny: clicked
]
