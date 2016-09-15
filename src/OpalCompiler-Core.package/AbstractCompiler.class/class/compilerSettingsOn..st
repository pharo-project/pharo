settings
compilerSettingsOn: aBuilder
	<systemsettings>
	(aBuilder group: #compiler)
		label: 'Compiler';
		description: 'All Compiler Settings';
		with: [ (aBuilder setting: #compileWithErrors) 
						label: 'Allow code with syntax errors';
						description: 'If checked then code with syntax errros can be compiled';
						target: self.
					(aBuilder pickOne: #compilerClass)
						target: SmalltalkImage;
						label: 'Default Compiler';
						domainValues: AbstractCompiler subclasses.
					(aBuilder pickOne: #bytecodeBackend)
						target: CompilationContext;
						label: 'Bytecode Backend';
						domainValues:
								{#V3PlusClosures -> OpalEncoderForV3PlusClosures.
								#SistaV1 -> OpalEncoderForSistaV1 }.
					(aBuilder setting: #usesFullBlockClosure)
						target: CompilationContext;
						label: 'FullBlockClosure';
						default: CompilationContext usesFullBlockClosure.]