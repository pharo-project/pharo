Class {
	#name : 'BaselineOfKernel',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfKernel',
	#package : 'BaselineOfKernel'
}

{ #category : 'accessing' }
BaselineOfKernel class >> additionalPackagesPackageNames [

	^ self packagesOfGroupNamed: #AdditionalPackages
]

{ #category : 'accessing' }
BaselineOfKernel class >> corePackageNames [

	^ self packagesOfGroupNamed: #Core
]

{ #category : 'accessing' }
BaselineOfKernel class >> testsPackageNames [

	^ self packagesOfGroupNamed: #Tests
]

{ #category : 'baselines' }
BaselineOfKernel >> baseline: spec [

	<baseline>
	| repository |
	repository := self packageRepositoryURLForSpec: spec.

	spec for: #common do: [
			spec
				baseline: 'Files-Tests' with: [
						spec
							className: 'BaselineOfFiles';
							repository: repository;
							loads: #( 'Tests' ) ];
				baseline: 'Zinc-Character-Encoding-Tests' with: [
					spec
						className: 'BaselineOfZincCharacterEncoding';
						repository: repository;
						loads: #( 'Tests' ) ];
				baseline: 'System-Platforms-Tests' with: [
					spec
						className: 'BaselineOfSystemPlatforms';
						repository: repository;
						loads: #( 'Tests' ) ];
				baseline: 'Collections-Tests' with: [
					spec
						className: 'BaselineOfCollections';
						repository: repository;
						loads: #('KernelTests') ];

				baseline: 'Announcements-Core-Tests' with: [
					spec
						className: 'BaselineOfAnnouncements';
						repository: repository;
						loads: #( 'Tests' ) ];

				baseline: 'SystemFinalization' with: [
					spec
						repository: repository;
						loads: #( 'Tests' ) ];

				baseline: 'System-Version' with: [
					spec
						className: 'BaselineOfSystemVersion';
						repository: repository;
						loads: 'Core' ];
				project: 'System-Version-Tests'
					copyFrom: 'System-Version'
					with: [ spec loads: 'Tests' ];

				package: 'Announcements-Core';
				package: 'Collections-Abstract';
				package: 'Collections-DoubleLinkedList';
				package: 'Collections-Native';
				package: 'Collections-Sequenceable';
				package: 'Collections-Streams';
				package: 'Collections-Strings';
				package: 'Collections-Support';
				package: 'Collections-Unordered';
				package: 'Collections-Weak';
				package: 'Files';
				package: 'Hermes';
				package: 'Kernel';
				package: 'Kernel-CodeModel';
				package: 'Kernel-BytecodeEncoders';
				package: 'Transcript-NonInteractive';
				package: 'PharoBootstrap-Initialization';
				package: 'Shift-ClassBuilder';
				package: 'System-Announcements';
				package: 'System-CommandLineHandler';
				package: 'System-Finalization';
				package: 'System-Platforms';
				package: 'System-SessionManager';
				package: 'System-Support';
				package: 'Zinc-Character-Encoding-Core';
				package: 'Hermes-Extensions';
				package: 'Hermes-Extensions-Tests' with: [ spec requires: #( 'Hermes-Extensions' ) ];
				package: 'Clap-Core';
				package: 'Clap-Commands-Pharo';
				package: 'NumberParser';
				package: 'ReflectionMirrors-Primitives';
				package: 'System-Time';
				package: 'Math-Operations-Extensions';
				package: 'System-NumberPrinting';
				package: 'Multilingual-Encodings';
				package: 'Kernel-Tests';
				package: 'Kernel-CodeModel-Tests';
				package: 'System-Time-Tests'. "The order of packages in the groups are important for the bootstrap! It define the load order."
			spec
				group: 'Core'
				with:
					{ 'Announcements-Core'. 'Collections-Abstract'. 'Collections-DoubleLinkedList'. 'Collections-Native'. 'Collections-Sequenceable'.
					'Collections-Streams'. 'Collections-Strings'. 'Collections-Support'. 'Collections-Unordered'. 'Collections-Weak'.
					'Files'. 'Hermes'. 'Kernel'. 'Kernel-CodeModel'. 'Kernel-BytecodeEncoders'. 'Transcript-NonInteractive'.
					'PharoBootstrap-Initialization'. 'Shift-ClassBuilder'. 'System-Announcements'. 'System-CommandLineHandler'.
					'System-Finalization'. 'System-Platforms'. 'System-SessionManager'. 'System-Support'. 'Zinc-Character-Encoding-Core' };
				group: 'AdditionalPackages'
				with: { 'Clap-Core'. 'Clap-Commands-Pharo'. 'Hermes-Extensions'. 'NumberParser'. 'ReflectionMirrors-Primitives'.
					'System-Time'. 'Math-Operations-Extensions'. 'System-NumberPrinting'. 'System-Version'. 'Multilingual-Encodings' };
				group: 'Tests'
				with:
					{ 'Collections-Tests'. 'Kernel-Tests'. 'Kernel-CodeModel-Tests'. 'Announcements-Core-Tests'. 'System-Platforms-Tests'.
					 'Files-Tests'. 'SystemFinalization'. 'Zinc-Character-Encoding-Tests'. 'System-Time-Tests' . 'Hermes-Extensions-Tests' . 'System-Version-Tests' } ]
]
