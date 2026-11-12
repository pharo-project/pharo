"
A test baseline asked twice with different groups: Core by TestGroups, Tests by TestGroupsLate (like Monticello, asked RemoteRepositories by BaselineOfPharo and Tests by BaselineOfIDE).
"
Class {
	#name : 'BaselineOfTestGroupsShared',
	#superclass : 'BaselineOf',
	#category : 'Metacello-Tests-Specs',
	#package : 'Metacello-Tests',
	#tag : 'Specs'
}

{ #category : 'baselines' }
BaselineOfTestGroupsShared >> baseline: spec [
	<baseline>
	spec for: #common do: [
			spec
				preLoadDoIt: #preLoad;
				postLoadDoIt: #postLoad.
			spec
				package: 'SharedCore';
				package: 'SharedTests'.
			spec
				group: 'Core' with: #( 'SharedCore' );
				group: 'Tests' with: #( 'SharedTests' ) ]
]

{ #category : 'actions' }
BaselineOfTestGroupsShared >> postLoad [
]

{ #category : 'actions' }
BaselineOfTestGroupsShared >> preLoad [
]
