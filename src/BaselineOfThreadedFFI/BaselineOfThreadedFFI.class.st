Class {
	#name : 'BaselineOfThreadedFFI',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfThreadedFFI',
	#package : 'BaselineOfThreadedFFI'
}

{ #category : 'accessing' }
BaselineOfThreadedFFI class >> corePackageNames [

	^ self packagesOfGroupNamed: #Core
]

{ #category : 'baseline' }
BaselineOfThreadedFFI >> baseline: spec [
	<baseline>

	spec for: #common do: [

		spec postLoadDoIt: #reset.

		spec
			"core"
			package: 'ThreadedFFI';
			package: 'ThreadedFFI-Graphics-Extensions' with: [ spec requires: #('ThreadedFFI') ];
			package: 'ThreadedFFI-UFFI' with: [ spec requires: #('ThreadedFFI') ];
			package: 'ThreadedFFI-UFFI-Overrides' with: [ spec requires: #('ThreadedFFI-UFFI') ];
			"tests"
			package: 'ThreadedFFI-Tests' with: [ spec requires: #('ThreadedFFI') ];
			package: 'ThreadedFFI-UFFI-Tests' with: [ spec requires: #('ThreadedFFI-UFFI' 'ThreadedFFI-UFFI-Overrides') ] ].		
	
	spec 
		group: 'Core' with: #('ThreadedFFI' 'ThreadedFFI-UFFI' 'ThreadedFFI-UFFI-Overrides');
		group: 'Graphics' with: #('Core' 'ThreadedFFI-Graphics-Extensions');
		group: 'Tests' with: #('ThreadedFFI-Tests' 'ThreadedFFI-UFFI-Tests');
		group: 'default' with: #('Core' 'Graphics' 'Tests')
]

{ #category : 'doits' }
BaselineOfThreadedFFI >> reset [

	Smalltalk globals
		at: #FFIMethodRegistry 
		ifPresent: [ :class | class resetAll ].
		
	Smalltalk globals
		at: #FFIBackend 
		ifPresent: [ :class | class reset ]
]
