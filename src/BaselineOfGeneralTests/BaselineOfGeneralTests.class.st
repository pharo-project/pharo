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
				baseline: 'SessionManagerTests' with: [
						spec
							className: 'BaselineOfSystemSessionManager';
							repository: repository;
							loads: #( 'Tests' ) ];
				baseline: 'System-CommandLineHandler-Tests' with: [
					spec
						className: 'BaselineOfSystemCommandLineHandler';
						repository: repository;
						loads: #( 'Tests' ) ];
				baseline: 'System-Support-Tests' with: [
					spec
						className: 'BaselineOfSystemSupport';
						repository: repository;
						loads: #( 'Tests' ) ];
				package: 'NumberParser-Tests';
				package: 'Fuel-Core-Tests';
				package: 'FormCanvas-Tests';
				package: 'Collections-Arithmetic-Tests';
				package: 'Metacello-CommandLine-Tests';
				baseline: 'Epicea' with: [ spec loads: #Tests from: repository ];
				baseline: 'Fonts' with: [ spec loads: #Tests from: repository ];
				baseline: 'FreeType' with: [ spec loads: #tests from: repository ];
				package: 'Graphics-Tests';
				package: 'Text-Tests';
				package: 'Morphic-Testing-Support';
				package: 'Morphic-Tests';
				package: 'Morphic-Widgets-FastTable-Tests';
				package: 'NECompletion-Tests';
				package: 'PragmaCollector-Tests';
				package: 'Regex-Core-Tests';
				package: 'Rubric-Tests';
				package: 'ScriptingExtensions-Tests';
				package: 'StartupPreferences-Tests';
				package: 'STON-Tests';
				package: 'System-Object Events-Tests';
				package: 'System-Model-Tests';
				package: 'System-Caching-Tests';
				package: 'System-Utilities-Tests';
				package: 'System-Hashing-Tests';
				package: 'System-History-Tests';
				package: 'System-Localization-Tests';
				package: 'System-Settings-Tests';
				package: 'Text-Diff-Tests';
				package: 'Tool-ExternalBrowser-Tests';
				package: 'Tool-Profilers-Tests';
				package: 'Tool-ImageCleaner-Tests';
				package: 'Tools-CodeNavigation-Tests';
				package: 'Tools-Tests';
				package: 'Zinc-Resource-Meta-Tests';
				package: 'Zinc-HTTP-Examples';
				package: 'Zinc-Tests';
				"requires Zinc-Resource-Meta-Tests"package: 'System-Identification-Tests';
				package: 'Transcript-NonInteractive-Tests';
				package: 'PharoDocComment-Tests' "standalone" ]
]
