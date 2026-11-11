"
A test baseline: TestGroups asks TestGroupsShared for its Core group, then requires TestGroupsLate, which asks TestGroupsShared for its Tests group.
"
Class {
	#name : 'BaselineOfTestGroupsLate',
	#superclass : 'BaselineOf',
	#category : 'Metacello-Tests-Specs',
	#package : 'Metacello-Tests',
	#tag : 'Specs'
}

{ #category : 'baselines' }
BaselineOfTestGroupsLate >> baseline: spec [

	<baseline>
	spec for: #common do: [
			spec package: 'LatePackage'.
			spec baseline: 'TestGroupsShared' with: [
					spec
						repository: 'http://repo.com';
						loads: 'Tests' ].
			spec package: 'LateTests' with: [ spec requires: #( 'TestGroupsShared' ) ] ]
]
