"
This baseline describes packages included in the bootstrapped Pharo image. 

It is not supposed to be part of bootstrapped image because it is dependent on Metacello.

This baseline includes several utility methods required to prepare data for the bootstrapping and initial image initialization.
"
Class {
	#name : #BaselineOfPharoBootstrap,
	#superclass : #BaselineOf,
	#category : #BaselineOfPharoBootstrap
}

{ #category : #accessing }
BaselineOfPharoBootstrap class >> compilerPackageNames [

	^ self packagesOfGroupNamed: #CompilerGroup
]

{ #category : #accessing }
BaselineOfPharoBootstrap class >> fileSystemPackageNames [

	^ self packagesOfGroupNamed: #FileSystemGroup
]

{ #category : #accessing }
BaselineOfPharoBootstrap class >> kernelPackageNames [

	^ self packagesOfGroupNamed: #KernelGroup
]

{ #category : #accessing }
BaselineOfPharoBootstrap class >> multilingualPackageNames [

	^ self packagesOfGroupNamed: #MultilingualGroup
]

{ #category : #accessing }
BaselineOfPharoBootstrap class >> sUnitPackageNames [

	^ self packagesOfGroupNamed: #SUnitGroup
]

{ #category : #baseline }
BaselineOfPharoBootstrap >> baseline: spec [
	<baseline>

	spec for: #common do: [
		spec package: 'AST-Core'.
		spec package: 'Announcements-Core'.
		spec package: 'Colors'.
		spec package: 'Collections-Abstract'.
		spec package: 'Collections-Arithmetic'.
		spec package: 'Collections-Atomic'.
		spec package: 'Collections-Native'.
		spec package: 'Collections-Sequenceable'.
		spec package: 'Collections-Stack'.
		spec package: 'Collections-Streams'.
		spec package: 'Collections-Strings'.
		spec package: 'Collections-Support'.
		spec package: 'Collections-Unordered'.
		spec package: 'Collections-Weak'.
		spec package: 'Files'.
		spec package: 'FileSystem-Path'.
		spec package: 'Hermes'.
		spec package: 'Jobs'.
		spec package: 'Kernel'.
		spec package: 'Kernel-BytecodeEncoders'.
		spec package: 'Network-UUID'.
		spec package: 'Transcript-NonInteractive'.
		spec package: 'PharoBootstrap-Initialization'.
		spec package: 'RPackage-Core'.
		spec package: 'Random-Core'.
		spec package: 'Shift-Changes'.
		spec package: 'Shift-ClassBuilder'.
		spec package: 'Shift-ClassInstaller'.
		spec package: 'Slot'.
		spec package: 'System-Announcements'.
		spec package: 'System-BasicCommandLineHandler'.
		spec package: 'System-CommandLine'.
		spec package: 'System-CommandLineHandler'.
		spec package: 'System-Finalization'.
		spec package: 'System-Hashing'.
		spec package: 'System-Model'.
		spec package: 'System-Platforms'.
		spec package: 'System-SessionManager'.
		spec package: 'System-Sources'.
		spec package: 'System-Support'.
		spec package: 'Traits'.
		spec package: 'Transcript-Core'.
		spec package: 'UIManager'.
		spec package: 'Zinc-Character-Encoding-Core'.
		
		spec package: 'FFI-Kernel'.
		spec package: 'FFI-Pools'.
		
		spec package: 'CodeExport'.
		spec package: 'CodeImport'.
		spec package: 'CodeImportCommandLineHandlers'.
		spec package: 'OpalCompiler-Core'.
		
		spec package: 'FileSystem-Core'.
		spec package: 'FileSystem-Disk'.
		spec package: 'DeprecatedFileStream'.

		spec package: 'SUnit-Core'.
		spec package: 'SUnit-Tests'.
		spec package: 'JenkinsTools-Core'.
		spec package: 'InitializePackagesCommandLineHandler'.
		

		spec group: 'KernelGroup' with: {
			'FFI-Kernel'.
			'FFI-Pools'.
		
			'AST-Core'.
			'Announcements-Core'.
			'Colors'.
			'Collections-Abstract'.
			'Collections-Arithmetic'.
			'Collections-Atomic'.
			'Collections-Native'.
			'Collections-Sequenceable'.
			'Collections-Stack'.
			'Collections-Streams'.
			'Collections-Strings'.
			'Collections-Support'.
			'Collections-Unordered'.
			'Collections-Weak'.
			'Files'.
			'FileSystem-Path'.
			'Hermes'.
			'Jobs'.
			'Kernel'.
			'Kernel-BytecodeEncoders'.
			'Network-UUID'.
			'Transcript-NonInteractive'.
			'PharoBootstrap-Initialization'.
			'RPackage-Core'.
			'Random-Core'.
			'Shift-Changes'.
			'Shift-ClassBuilder'.
			'Shift-ClassInstaller'.
			'Slot'.
			'System-Announcements'.
			'System-BasicCommandLineHandler'.
			'System-CommandLine'.
			'System-CommandLineHandler'.
			'System-Finalization'.
			'System-Hashing'.
			'System-Model'.
			'System-Platforms'.
			'System-SessionManager'.
			'System-Sources'.
			'System-Support'.
			'Traits'.
			'Transcript-Core'.
			'UIManager'.
			'Zinc-Character-Encoding-Core'.
			'InitializePackagesCommandLineHandler'.
			
			'Multilingual-Languages'.
			'Multilingual-Encodings'.
			'Multilingual-TextConversion' }.

		"These packages are added using hermes after bootstrap"
		spec group: 'CompilerGroup' with: {
			'CodeExport'.
			'CodeImport'.
			'CodeImportCommandLineHandlers'.
			'OpalCompiler-Core' }.
		
		"These packages are added using the compiler after bootstrap"
		spec group: 'FileSystemGroup' with: {
			'FileSystem-Core'.
			'FileSystem-Disk'.
			'DeprecatedFileStream' }.

		spec group: 'SUnitGroup' with: {
			'SUnit-Core'.
			'SUnit-Tests'.
			'JenkinsTools-Core'}.

		spec group: 'MultilingualGroup' with: {
			'Multilingual-Languages'.
			'Multilingual-Encodings'.
			'Multilingual-TextConversion'}.	]

]
