"
A test baseline: TestGroups asks TestGroupsShared for its Core group, then requires TestGroupsLate, which asks TestGroupsShared for its Tests group.
"
Class {
	#name : 'BaselineOfTestGroups',
	#superclass : 'BaselineOf',
	#category : 'Metacello-Tests-Specs',
	#package : 'Metacello-Tests',
	#tag : 'Specs'
}

{ #category : 'baselines' }
BaselineOfTestGroups >> baseline: spec [

	<baseline>
	spec for: #common do: [
			spec baseline: 'TestGroupsShared' with: [
					spec
						repository: 'http://repo.com';
						loads: 'Core' ].
			spec baseline: 'TestGroupsLate' with: [ spec repository: 'http://repo.com' ] ]
]
