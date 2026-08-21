Class {
	#name : 'BaselineOfCompiler',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfCompiler',
	#package : 'BaselineOfCompiler'
}

{ #category : 'accessing' }
BaselineOfCompiler class >> corePackageNames [

	^ self packagesOfGroupNamed: #Core
]

{ #category : 'accessing' }
BaselineOfCompiler class >> testsPackageNames [

	^ self packagesOfGroupNamed: #Tests
]

{ #category : 'baselines' }
BaselineOfCompiler >> baseline: spec [
	<baseline>	
	| repository |
	repository := self packageRepositoryURLForSpec: spec.

	spec for: #common do: [
			spec baseline: 'CollectionsTests' with: [
            spec
                repository: repository;
                className: 'BaselineOfCollections';
                loads: #( 'Collections-Abstract-Tests' 'Collections-Unordered-Tests' 'Collections-Support-Tests' ) ].

			spec baseline: 'Collections' with: [
            spec
                repository: repository;
                className: 'BaselineOfCollections';
                loads: #( 'Collections-Stack' 'Collections-Atomic' 'Collections-Arithmetic' ) ].
		
			spec baseline: 'KernelTests' with: [
				spec
					repository: repository;
					className: 'BaselineOfKernel';
					loads: #(  'Kernel-Tests' 'System-Time-Tests') ].

			spec baseline: 'CoreTraits' with: [
				spec
					repository: repository;
					className: 'BaselineOfTraits';
					loads: #( 'core-traits' ) ].
			
			spec baseline: 'ClapTests' with: [
            spec
                repository: repository;
					 className: 'BaselineOfClap';
                loads: #( 'Tests' ) ].

			spec
				package: 'ClassDefinitionPrinters';

				package: 'CodeExport';
				package: 'CodeImport';
				package: 'CodeImport-Commands';
				package: 'CodeImport-Tests' with: [ spec requires: #( 'AST-Core-Tests' 'ClapTests' ) ];

				package: 'ParseTreeRewriter';
				package: 'Deprecation';

				package: 'AST-Core';
				package: 'OpalCompiler-Core';
				package: 'DebugInfo';

				package: 'Kernel-Extended-Tests' with: [ spec requires: #( 'KernelTests' 'CollectionsTests' 'CoreTraits' ) ];
				package: 'Kernel-Tests-WithCompiler' with: [ spec requires: #( 'KernelTests' 'CollectionsTests') ];

				package: 'AST-Core-Tests';

				package: 'OpalCompiler-Tests' with: [ spec requires: #( 'AST-Core-Tests' ) ];
				package: 'DebugInfo-Tests' with: [ spec requires: #( 'AST-Core-Tests' ) ].

			spec
				group: 'Core'
				with: { 'AST-Core'. 'Collections-Arithmetic'. 'Collections-Atomic'. 'Collections-Stack'. 'ClassDefinitionPrinters'.
					'CodeExport'. 'CodeImport'. 'CodeImport-Commands'. 'DebugInfo'. 'OpalCompiler-Core'. 'ParseTreeRewriter'. 'Deprecation' };
				group: 'Tests'
				with: { 'Kernel-Extended-Tests'. 'Kernel-Tests-WithCompiler'.
					'AST-Core-Tests'. 'OpalCompiler-Tests'. 'DebugInfo-Tests'. 'CodeImport-Tests' } ]
]
