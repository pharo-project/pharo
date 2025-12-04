Class {
	#name : 'BaselineOfGeneralTests',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfGeneralTests',
	#package : 'BaselineOfGeneralTests'
}

{ #category : 'baselines' }
BaselineOfGeneralTests >> baseline: spec [
	"Needs to be re-sorted"

	<baseline>
	| repository |
	repository := self packageRepositoryURLForSpec: spec.
	spec for: #common do: [
		spec
			baseline: 'Debugging' with: [ spec loads: #Tests from: repository ];
			package: 'NumberParser-Tests';
			package: 'AST-Core-Tests';
			package: 'Kernel-CodeModel-Tests';
			package: 'Monticello-Tests';
			package: 'MonticelloMocks';
			package: 'Fuel-Core-Tests';
			package: 'FormCanvas-Tests';
			package: 'Collections-DoubleLinkedList-Tests';
			package: 'Collections-Arithmetic-Tests';
			package: 'Metacello-CommandLine-Tests';
			baseline: 'Epicea' with: [
				spec
					repository: repository;
					loads: #Tests ];
			package: 'FileSystem-Memory-Tests';
			baseline: 'Fonts' with: [ spec loads: #Tests from: repository ];
			baseline: 'FreeType' with: [ spec loads: #tests from: repository ];
			package: 'Graphics-Tests';
			package: 'Text-Tests';
			package: 'Morphic-Testing-Support';
			package: 'Morphic-Tests';
			package: 'Morphic-Widgets-FastTable-Tests';
			package: 'NECompletion-Tests';

			package: 'Regex-Core-Tests';
			package: 'Ring-Definitions-Monticello-Tests';
			package: 'Rubric-Tests';
			package: 'ScriptingExtensions-Tests';
			package: 'STON-Tests';
			package: 'System-Object Events-Tests';
			package: 'System-Caching-Tests';
			package: 'System-Utilities-Tests';
			package: 'System-Hashing-Tests';
			package: 'System-History-Tests';
			package: 'System-Localization-Tests';
			package: 'System-SessionManager-Tests';
			package: 'System-Settings-Tests';
			package: 'System-Support-Tests';
			package: 'Text-Diff-Tests';
			package: 'Tool-ExternalBrowser-Tests';
			package: 'Tool-Profilers-Tests';
			package: 'Tool-ImageCleaner-Tests';
			package: 'Tools-CodeNavigation-Tests';
			package: 'Tools-Tests';
			package: 'Zinc-Resource-Meta-Tests';
			package: 'Zinc-HTTP-Examples';
			package: 'Zinc-Tests';
			"requires Zinc-Resource-Meta-Tests"package:
			'System-Identification-Tests';
			package: 'System-Dependencies-Tests';
			package: 'System-CommandLineHandler-Tests';
			package: 'Transcript-NonInteractive-Tests';
			package: 'PharoDocComment-Tests' "standalone" ]
]
