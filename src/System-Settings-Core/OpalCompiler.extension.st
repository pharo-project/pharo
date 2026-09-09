Extension { #name : 'OpalCompiler' }

{ #category : '*System-Settings-Core' }
OpalCompiler class >> compilerSettingsOn: aBuilder [
	<systemsettings>
	(aBuilder group: #compiler)
		label: 'Compiler';
		description: 'All Compiler Settings';
		with: [
				(aBuilder setting: #createAndEnableOverlay)
						label: 'Compiler Overlay Environment';
						target: self;
						default: false;
						description: 'Create a overlay environment for the OpalCompiler package and sets this copy as the global compiler. After this all classes in the package OpalCompiler are not used anymore outside of tests.  Simplifies changing the compiler'.
				OCCompilationContext compilerSettingsOn: aBuilder]
]
