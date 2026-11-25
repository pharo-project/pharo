"
Baseline managing the Flashback project for the Pharo bootstrap.

Load type: 
- linear
"
Class {
	#name : 'BaselineOfFlashback',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfFlashback',
	#package : 'BaselineOfFlashback'
}

{ #category : 'baseline' }
BaselineOfFlashback >> baseline: spec [
	<baseline>
	| repository |
	repository := self packageRepositoryURLForSpec: spec.
	spec
		for: #common
		do: [ 
			"The tests decompile the examples of the compiler's tests"
			spec baseline: 'CompilerTests' with: [
				spec
					repository: repository;
					className: 'BaselineOfCompiler';
					loads: #( 'OpalCompiler-Tests' ) ].

			"Packages"
			spec package: 'Flashback-Decompiler'.
			spec package: 'Flashback-Decompiler-Tests' with: [ spec requires: #( 'Flashback-Decompiler' 'CompilerTests' ) ].

			"Groups"
			spec
				group: 'Core' with: #('Flashback-Decompiler');
				group: 'Tests' with: #('Flashback-Decompiler-Tests') ]
]
