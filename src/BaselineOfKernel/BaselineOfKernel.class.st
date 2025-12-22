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
	spec for: #common do: [
			spec
				package: 'FFI-Kernel';
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
				package: 'ClassDefinitionPrinters';
				package: 'CodeExport';
				package: 'System-Sources';
				package: 'System-Support';
				package: 'Zinc-Character-Encoding-Core';
				
				package: 'Hermes-Extensions';
				package: 'Clap-Core';
				package: 'Clap-Commands-Pharo';
				package: 'NumberParser';
				package: 'ReflectionMirrors-Primitives';
				package: 'System-Time';
				package: 'Math-Operations-Extensions';
				package: 'System-NumberPrinting';
				package: 'System-Version';
				package: 'Multilingual-Encodings';
				
				package: 'Collections-Abstract-Tests';
				package: 'Collections-DoubleLinkedList-Tests';
				package: 'Collections-Native-Tests';
				package: 'Collections-Sequenceable-Tests';
				package: 'Collections-Streams-Tests';
				package: 'Collections-Strings-Tests';
				package: 'Collections-Support-Tests';
				package: 'Collections-Unordered-Tests';
				package: 'Collections-Weak-Tests' with: [ spec requires: #( 'Collections-Unordered-Tests' ) ];
				package: 'Kernel-Tests';
				package: 'Kernel-CodeModel-Tests';
				package: 'Files-Tests';
				package: 'Announcements-Core-Tests';
				package: 'System-Platforms-Tests';
				package: 'System-Finalization-Tests';
				package: 'Zinc-Character-Encoding-Tests';
				package: 'System-Time-Tests'.

			"The order of packages in the groups are important for the bootstrap! It define the load order."
			spec
				group: 'Core'
				with: { 'FFI-Kernel'. 'Announcements-Core'. 'Collections-Abstract'. 'Collections-DoubleLinkedList'. 'Collections-Native'. 'Collections-Sequenceable'.
					'Collections-Streams'. 'Collections-Strings'. 'Collections-Support'. 'Collections-Unordered'. 'Collections-Weak'. 'Files'. 'Hermes'. 'Kernel'. 'Kernel-CodeModel'.
					'Kernel-BytecodeEncoders'. 'Transcript-NonInteractive'. 'PharoBootstrap-Initialization'. 'Shift-ClassBuilder'. 'System-Announcements'.
					'System-CommandLineHandler'. 'System-Finalization'. 'System-Platforms'. 'System-SessionManager'. 'ClassDefinitionPrinters'. 'CodeExport'. 'System-Sources'.
					'System-Support'. 'Zinc-Character-Encoding-Core' };
				group: 'AdditionalPackages'
				with: { 'Clap-Core'. 'Clap-Commands-Pharo'. 'Hermes-Extensions'. 'NumberParser'. 'ReflectionMirrors-Primitives'. 'System-Time'. 'Math-Operations-Extensions'.
					'System-NumberPrinting'. 'System-Version'. 'Multilingual-Encodings' };
				group: 'Tests'
				with:
					{ 'Collections-Abstract-Tests'. 'Collections-DoubleLinkedList-Tests'. 'Collections-Native-Tests'. 'Collections-Sequenceable-Tests'. 'Collections-Streams-Tests'. 'Collections-Strings-Tests'.
					'Collections-Support-Tests'. 'Collections-Unordered-Tests'. 'Collections-Weak-Tests'. 'Kernel-Tests'. 'Kernel-CodeModel-Tests'. 'Announcements-Core-Tests'. 'System-Platforms-Tests'.
					 'Files-Tests'. 'System-Finalization-Tests'. 'Zinc-Character-Encoding-Tests'. 'System-Time-Tests' } ]
]
