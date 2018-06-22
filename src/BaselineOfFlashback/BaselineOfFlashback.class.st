"
Baseline managing the Flashback project for the Pharo bootstrap.

Load type: 
- linear
"
Class {
	#name : #BaselineOfFlashback,
	#superclass : #BaselineOf,
	#category : #BaselineOfFlashback
}

{ #category : #baseline }
BaselineOfFlashback >> baseline: spec [
	<baseline>
	spec
		for: #common
		do: [ 
			"Packages"
			spec package: 'Flashback-Decompiler'.
			spec package: 'Flashback-Decompiler-Tests' with: [ spec requires: 'Flashback-Decompiler' ].

			"Groups"
			spec
				group: 'Core' with: #('Flashback-Decompiler');
				group: 'Tests' with: #('Flashback-Decompiler-Tests') ]
]
