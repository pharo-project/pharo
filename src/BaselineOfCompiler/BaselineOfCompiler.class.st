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
	spec for: #common do: [
			spec baseline: 'KernelTests' with: [
				spec
					repository: (self packageRepositoryURLForSpec: spec);
					className: 'BaselineOfKernel';
					loads: #( 'Collections-Abstract-Tests' 'Kernel-Tests' 'Collections-Unordered-Tests' 'Collections-Support-Tests' 'System-Time-Tests') ].

			spec baseline: 'CoreTraits' with: [
				spec
					repository: (self packageRepositoryURLForSpec: spec);
					className: 'BaselineOfTraits';
					loads: #( 'core-traits' ) ].

			spec

				package: 'Collections-Arithmetic';
				package: 'Collections-Atomic';
				package: 'Collections-Stack';
				package: 'Collections-Atomic-Tests';
				package: 'Collections-Stack-Tests' with: [ spec requires: #( 'KernelTests' ) ];

				package: 'ClassDefinitionPrinters';

				package: 'CodeExport';
				package: 'CodeImport';
				package: 'CodeImport-Commands';
				package: 'CodeImport-Tests' with: [ spec requires: #( 'AST-Core-Tests' ) ];

				package: 'ParseTreeRewriter';
				package: 'Deprecation';

				package: 'AST-Core';
				package: 'OpalCompiler-Core';
				package: 'DebugInfo';

				package: 'Kernel-Extended-Tests' with: [ spec requires: #( 'KernelTests' 'CoreTraits' ) ];
				package: 'Kernel-Tests-WithCompiler' with: [ spec requires: #( 'KernelTests' ) ];

				package: 'AST-Core-Tests';

				package: 'OpalCompiler-Tests' with: [ spec requires: #( 'AST-Core-Tests' ) ];
				package: 'DebugInfo-Tests' with: [ spec requires: #( 'AST-Core-Tests' ) ].

			spec
				group: 'Core'
				with: { 'AST-Core'. 'Collections-Arithmetic'. 'Collections-Atomic'. 'Collections-Stack'. 'ClassDefinitionPrinters'.
					'CodeExport'. 'CodeImport'. 'CodeImport-Commands'. 'DebugInfo'. 'OpalCompiler-Core'. 'ParseTreeRewriter'. 'Deprecation' };
				group: 'Tests'
				with: { 'Collections-Atomic-Tests'. 'Collections-Stack-Tests'. 'Kernel-Extended-Tests'. 'Kernel-Tests-WithCompiler'.
					'AST-Core-Tests'. 'OpalCompiler-Tests'. 'DebugInfo-Tests'. 'CodeImport-Tests' } ]
]
