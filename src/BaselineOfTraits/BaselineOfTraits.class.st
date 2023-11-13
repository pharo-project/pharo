"
I am the baseline for loading Traits in a bootstraped image.
Usually  my core is loaded during the bootstrap process by Hermes. 
The other parts are loaded after using Metacello.

However  I have the following groups to describe the dependencies:

- core : is the minimal core implementation of Traits. Loaded by Hermes
- traits-in-kernel : I include all the traits that are part of the bootstraped kernel. Also loaded by Hermes.
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
	| groups |

	groups := self packagesOfGroupNamed: 'core-traits'.
	^ groups flatCollect: [ :e | self packagesOfGroupNamed: e ].
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
			
			package: 'Kernel-Traits' with: [ spec requires: #('Traits') ];
			package: 'Collections-Abstract-Traits' with: [ spec requires: #('Traits') ];
			package: 'CodeImport-Traits' with: [ spec requires: #('Traits') ];
			package: 'CodeExport-Traits' with: [ spec requires: #('Traits') ];
			package: 'Traits-Tests' with: [ spec requires: #('Traits') ];
			package: 'Traits-Compatibility' with: [ spec requires: #('Traits') ].

		spec group: 'core' with: #('Traits').
					
		spec group: 'traits-in-kernel' with: #(
					'Kernel-Traits'
					'Collections-Abstract-Traits'
					'CodeImport-Traits'
					'CodeExport-Traits' ).
					
		spec group: 'compatibility' with: #('Traits-Compatibility').
		
		spec group: 'core-traits' with: #(core 'traits-in-kernel' 'compatibility').		
		spec group: 'default' with: #('core-traits').
		spec group: 'traits-tests' with: #('default' 'Traits-Tests').		
	 ].
]
