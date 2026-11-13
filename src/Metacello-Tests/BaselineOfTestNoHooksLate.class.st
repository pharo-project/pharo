Class {
	#name : 'BaselineOfTestNoHooksLate',
	#superclass : 'BaselineOf',
	#category : 'Metacello-Tests-Specs',
	#package : 'Metacello-Tests',
	#tag : 'Specs'
}

{ #category : 'baselines' }
BaselineOfTestNoHooksLate >> baseline: spec [
	<baseline>
	spec for: #common do: [
			spec baseline: 'TestNoHooksShared' with: [
					spec
						repository: 'http://repo.com';
						loads: 'Tests' ].
			spec package: 'NoHooksLatePackage' with: [ spec requires: #( 'TestNoHooksShared' ) ] ]
]
