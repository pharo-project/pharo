Class {
	#name : 'BaselineOfTestNoHooks',
	#superclass : 'BaselineOf',
	#category : 'Metacello-Tests-Specs',
	#package : 'Metacello-Tests',
	#tag : 'Specs'
}

{ #category : 'baselines' }
BaselineOfTestNoHooks >> baseline: spec [
	<baseline>
	spec for: #common do: [
			spec baseline: 'TestNoHooksShared' with: [
					spec
						repository: 'http://repo.com';
						loads: 'Core' ].
			spec baseline: 'TestNoHooksLate' with: [ spec repository: 'http://repo.com' ] ]
]
