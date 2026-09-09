Class {
	#name : 'BaselineOfKeymapping',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfKeymapping',
	#package : 'BaselineOfKeymapping'
}

{ #category : 'accessing' }
BaselineOfKeymapping class >> corePackageNames [

	^ self deepPackagesOfGroupNamed: #Core
]

{ #category : 'baselines' }
BaselineOfKeymapping >> baseline: spec [

	<baseline>
	spec for: #common do: [
			spec
				package: 'Keymapping-Core';
				package: 'Keymapping-KeyCombinations' with: [ spec requires: #( 'Keymapping-Core' ) ];
				package: 'Keymapping-Pragmas' with: [ spec requires: #( 'Keymapping-Core' ) ];
				package: 'Keymapping-Tests' with: [ spec requires: #( 'Keymapping-Core' 'Keymapping-KeyCombinations' ) ].

			spec
				group: 'Core' with: #( 'Keymapping-Core' 'Keymapping-KeyCombinations' 'Keymapping-Pragmas' );
				group: 'Tests' with: #( 'Core' 'Keymapping-Tests' ) ]
]
