"
I am the baseline for loading Traits in a bootstraped image.
Usually  my core is loaded during the bootstrap process by Hermes. 
The other parts are loaded after using Metacello.

However  I have the following groups to describe the dependencies:

- core : is the minimal core implementation of Traits. Loaded by Hermes
- compatibility: I include a compatibility package with the old traits and classes to be used only to accept the extension methods. This will be removed with the time.
- core-traits: Minimum required to use in an image.
- default: I include the tests and all the previous.
- traits-tests: adding the tests for Traits.
"
Class {
	#name : 'BaselineOfTraits',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfTraits',
	#package : 'BaselineOfTraits'
}

{ #category : 'baselines' }
BaselineOfTraits class >> corePackages [
	 ^ (self packagesOfGroupNamed: 'core-traits') , (self packagesOfGroupNamed: 'core')
]

{ #category : 'baselines' }
BaselineOfTraits class >> packagesToExportWithHermes [

	^ self allPackageNames reject: [ :aPackageName | aPackageName = 'Traits-Tests'].
]

{ #category : 'baselines' }
BaselineOfTraits >> baseline: spec [

	<baseline>
	spec for: #common do: [
		spec
			package: 'Traits';
			package: 'Kernel-Traits' with: [ spec requires: #( 'Traits' ) ];
			package: 'CodeImport-Traits' with: [ spec requires: #( 'Traits' ) ];
			package: 'Traits-Tests' with: [ spec requires: #( 'Traits' ) ].

		spec group: 'core' with: #( 'Traits' ).

		spec group: 'core-traits' with: #( 'Kernel-Traits' 'CodeImport-Traits' ).

		spec group: 'default' with: #( 'core-traits' ).
		spec group: 'traits-tests' with: #( 'default' 'Traits-Tests' ) ]
]
