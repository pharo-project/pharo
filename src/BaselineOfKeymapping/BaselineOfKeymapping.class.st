Class {
	#name : 'BaselineOfKeymapping',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfKeymapping',
	#package : 'BaselineOfKeymapping'
}

{ #category : 'baselines' }
BaselineOfKeymapping >> baseline: spec [

	<baseline>
	spec for: #common do: [
			spec preLoadDoIt: #preload:package:.

			spec
				package: 'Keymapping-Core';
				package: 'Keymapping-KeyCombinations';
				package: 'Keymapping-Pragmas';
				package: 'Keymapping-Tests'.

			spec
				group: 'default' with: #( 'core' 'ui' 'tests' );
				group: 'core' with: #( 'Keymapping-Core' 'Keymapping-KeyCombinations' );
				group: 'morphic' with: #( 'core' 'Keymapping-Pragmas' );
				group: 'ui' with: #( 'core' 'morphic' );
				group: 'tests' with: #( 'core' 'Keymapping-Tests' ) ]
]

{ #category : 'actions' }
BaselineOfKeymapping >> postload: loader package: packageSpec [

	"Once everything is initialized we can reset the keymap"
	KMPragmaKeymapBuilder uniqueInstance reset
]

{ #category : 'accessing' }
BaselineOfKeymapping >> project [
	| aProject |
	
	aProject := super project.
	aProject loadType: #atomic.

	^ aProject
]
