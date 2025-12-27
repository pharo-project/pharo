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
			spec
				package: 'AST-Core';
				package: 'Collections-Arithmetic';
				package: 'Collections-Atomic';
				package: 'Collections-Stack';
				package: 'CodeImport';
				package: 'CodeImport-Commands';
				package: 'DebugInfo';
				package: 'OpalCompiler-Core';
				package: 'ParseTreeRewriter';
				package: 'Deprecation';

				package: 'Collections-Atomic-Tests';
				package: 'Collections-Stack-Tests';
				package: 'Kernel-Extended-Tests';
				package: 'Kernel-Tests-WithCompiler';
				package: 'AST-Core-Tests';
				package: 'OpalCompiler-Tests' with: [ spec requires: #( 'AST-Core-Tests' ) ];
				package: 'DebugInfo-Tests' with: [ spec requires: #( 'AST-Core-Tests' ) ];
				package: 'CodeImport-Tests' with: [ spec requires: #( 'AST-Core-Tests' ) ].

			spec
				group: 'Core'
				with:
					{ 'AST-Core'. 'Collections-Arithmetic'. 'Collections-Atomic'. 'Collections-Stack'. 'CodeImport'. 'CodeImport-Commands'. 'DebugInfo'.
					'OpalCompiler-Core'. 'ParseTreeRewriter'. 'Deprecation' };
				group: 'Tests'
				with:
					{ 'Collections-Atomic-Tests'. 'Collections-Stack-Tests'. 'Kernel-Extended-Tests'. 'Kernel-Tests-WithCompiler'. 'AST-Core-Tests'. 'OpalCompiler-Tests'.
					'DebugInfo-Tests'. 'CodeImport-Tests' } ]
]
