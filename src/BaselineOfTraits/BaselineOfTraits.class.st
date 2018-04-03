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
- traits-tests: adding the tests for TraitsV2.
"
Class {
	#name : #BaselineOfTraits,
	#superclass : #BaselineOf,
	#category : #BaselineOfTraits
}

{ #category : #baselines }
BaselineOfTraits class >> corePackages [
	| groups |

	groups := self packagesOfGroupNamed: 'core-traits'.
	^ groups flatCollect: [ :e | self packagesOfGroupNamed: e ].
]

{ #category : #baselines }
BaselineOfTraits >> baseline: spec [
	<baseline>

	spec for: #common do: [

		spec 
			package: 'TraitsV2';
			
			package: 'Kernel-Traits' with: [ spec requires: #('TraitsV2') ];
			package: 'AST-Core-Traits' with: [ spec requires: #('TraitsV2') ];
			package: 'Collections-Abstract-Traits' with: [ spec requires: #('TraitsV2') ];
			package: 'Transcript-Core-Traits' with: [ spec requires: #('TraitsV2') ];
			package: 'SUnit-Core-Traits' with: [ spec requires: #('TraitsV2') ];
			package: 'CodeImport-Traits' with: [ spec requires: #('TraitsV2') ];
			package: 'OpalCompiler-Traits' with: [ spec requires: #('TraitsV2') ];
			package: 'Slot-Traits' with: [ spec requires: #('TraitsV2') ];
			package: 'CodeExport-Traits' with: [ spec requires: #('TraitsV2') ];
			package: 'System-Sources-Traits' with: [ spec requires: #('TraitsV2') ];
			package: 'System-Support-Traits' with: [ spec requires: #('TraitsV2') ];
			package: 'TraitsV2-Tests' with: [ spec requires: #('TraitsV2') ];
			package: 'TraitsV2-Compatibility' with: [ spec requires: #('TraitsV2') ].

		spec group: 'core' with: #('TraitsV2').
					
		spec group: 'traits-in-kernel' with: #(
					'Kernel-Traits'
					'AST-Core-Traits'
					'Collections-Abstract-Traits'
					'Transcript-Core-Traits'
					'CodeImport-Traits'
					'OpalCompiler-Traits'
					'Slot-Traits'
					'CodeExport-Traits'
					'System-Sources-Traits'
					'System-Support-Traits' ).
					
		spec group: 'compatibility' with: #('TraitsV2-Compatibility').
		
		spec group: 'core-traits' with: #(core 'traits-in-kernel' 'compatibility').		
		spec group: 'default' with: #('core-traits' 'SUnit-Core-Traits').
		spec group: 'traits-tests' with: #('default' 'TraitsV2-Tests').		
	 ].		

]
