"
This baseline describes packages included in the bootstrapped Pharo image. 

It is not supposed to be part of bootstrapped image because it is dependent on Metacello.

This baseline includes several utility methods required to prepare data for the bootstrapping and initial image initialization.
"
Class {
	#name : 'BaselineOfPharoBootstrap',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfPharoBootstrap',
	#package : 'BaselineOfPharoBootstrap'
}

{ #category : 'accessing' }
BaselineOfPharoBootstrap class >> compilerPackageNames [

	^ self packagesOfGroupNamed: #CompilerGroup
]

{ #category : 'accessing' }
BaselineOfPharoBootstrap class >> fileSystemPackageNames [

	^ self packagesOfGroupNamed: #FileSystemGroup
]

{ #category : 'accessing' }
BaselineOfPharoBootstrap class >> kernelAdditionalPackagesNames [

	^ self packagesOfGroupNamed: #AdditionalPackages
]

{ #category : 'accessing' }
BaselineOfPharoBootstrap class >> kernelPackageNames [

	^ self packagesOfGroupNamed: #KernelGroup
]

{ #category : 'accessing' }
BaselineOfPharoBootstrap class >> multilingualPackageNames [

	^ self packagesOfGroupNamed: #MultilingualGroup
]

{ #category : 'traversing' }
BaselineOfPharoBootstrap class >> packagesRecursiveIn: aGroupName [

	| allMembers allPackagesInBaseline packages groups |
	allMembers := self version groups detect: [ :g | g name = aGroupName ].
	allPackagesInBaseline := self version packages collect: [ :each | each name ].
	packages := allMembers includes select: [ :aName | allPackagesInBaseline includes: aName ].
	groups := allMembers includes reject: [ :aName | allPackagesInBaseline includes: aName ].		
	^ packages , (groups flatCollect: [ :aInnerGroupName | self packagesRecursiveIn: aInnerGroupName ]).
]

{ #category : 'accessing' }
BaselineOfPharoBootstrap class >> packagesToExportWithHermes [

	^ self packagesRecursiveIn: 'ToLoadByHermes'
]

{ #category : 'accessing' }
BaselineOfPharoBootstrap class >> sUnitPackageNames [

	^ self packagesOfGroupNamed: #SUnitGroup
]

{ #category : 'baselines' }
BaselineOfPharoBootstrap >> baseline: spec [
	<baseline>

	spec for: #common do: [
		spec package: 'AST-Core'.
		spec package: 'Announcements-Core'.
		spec package: 'Collections-Abstract'.
		spec package: 'Collections-Arithmetic'.
		spec package: 'Collections-Atomic'.
		spec package: 'Collections-DoubleLinkedList'.
		spec package: 'Collections-Native'.
		spec package: 'Collections-Sequenceable'.
		spec package: 'Collections-Stack'.
		spec package: 'Collections-Streams'.
		spec package: 'Collections-Strings'.
		spec package: 'Collections-Support'.
		spec package: 'Collections-Unordered'.
		spec package: 'Collections-Weak'.
		spec package: 'Debugging-Core'.
		spec package: 'Files'.
		spec package: 'FileSystem-Path'.
		spec package: 'Hermes'.
		spec package: 'Hermes-Extensions'.

		spec package: 'Kernel'.
		spec package: 'Kernel-CodeModel'.
		spec package: 'Kernel-BytecodeEncoders'.
		spec package: 'System-Time'.
		spec package: 'NumberParser'.

		spec package: 'Math-Operations-Extensions'.

		spec package: 'Multilingual-Encodings'.

		spec package: 'Network-UUID'.
		
		spec package: 'Transcript-NonInteractive'.
		spec package: 'PharoBootstrap-Initialization'.
		spec package: 'Random-Core'.
		spec package: 'ReflectionMirrors-Primitives'.
		spec package: 'Shift-ClassBuilder'.
		spec package: 'System-Announcements'.
		spec package: 'System-BasicCommandLineHandler'.
		spec package: 'System-CommandLine'.
		spec package: 'System-CommandLineHandler'.
		spec package: 'System-Finalization'.
		spec package: 'System-Hashing'.
		spec package: 'System-Platforms'.
		spec package: 'System-SessionManager'.
		spec package: 'System-Sources'.
		spec package: 'System-Support'.

		spec package: 'UIManager'.
		spec package: 'Zinc-Character-Encoding-Core'.
		
		spec package: 'FFI-Kernel'.
		
		"Minimal command line handler for bootstrap.
		Only using perform and environment #at:"
		spec package: 'PerformMessageCommandLineHandler'.
		
		spec package: 'CodeImport'.
		
		spec package: 'CodeImportCommandLineHandlers'.
		
		spec package: 'Debugging-Utils'.
		spec package: 'OpalCompiler-Core'.
		
		spec package: 'FileSystem-Core'.
		spec package: 'FileSystem-Disk'.

		spec package: 'System-NumberPrinting'.
		spec package: 'SUnit-Core'.
		spec package: 'SUnit-Tests'.
		spec package: 'Kernel-Tests'.
		spec package: 'JenkinsTools-Core'.
		spec package: 'System-Model'.
		spec package: 'InitializePackagesCommandLineHandler'.
		

		spec group: 'KernelGroup' with: {
			'FFI-Kernel'.
			'Announcements-Core'.
			'Collections-Abstract'.
			'Collections-DoubleLinkedList'.
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
			'Kernel'.
			'Kernel-CodeModel'.
			'Kernel-BytecodeEncoders'.
			'Transcript-NonInteractive'.
			'PharoBootstrap-Initialization'.
			'Shift-ClassBuilder'.
			'System-Announcements'.
			'System-BasicCommandLineHandler'.
			'System-CommandLine'.
			'System-CommandLineHandler'.
			'System-Finalization'.
			'System-Platforms'.
			'System-SessionManager'.
			'System-Sources'.
			
			'PerformMessageCommandLineHandler'.
			'System-Support'.
			'NumberParser'.

			'UIManager'.
			'Zinc-Character-Encoding-Core' }.

		"These packages are added using hermes after bootstrap"

		spec group: 'AdditionalPackages' with: {
			'ReflectionMirrors-Primitives'.
			'InitializePackagesCommandLineHandler'.
			'System-Time'.
			'Math-Operations-Extensions'.
			'Debugging-Core'.
			'System-NumberPrinting'.
		}.

		spec group: 'MultilingualGroup' with: {
			'Multilingual-Encodings' }.

		spec group: 'CompilerGroup' with: {
			'AST-Core'.
			'Collections-Arithmetic'.
			'Collections-Atomic'.
			'CodeImport'.
			'CodeImportCommandLineHandlers'.
			'Debugging-Utils'.
			'OpalCompiler-Core'}.
		
		spec group: 'FileSystemGroup' with: {
			'Random-Core'.
			'System-Hashing'.
			'Network-UUID'.
			'FileSystem-Core'.
			'FileSystem-Disk'}.

		spec group: 'ToLoadByHermes' with: {
			'AdditionalPackages'.
			'MultilingualGroup'.
			'CompilerGroup'.
			'FileSystemGroup'.
			'Hermes-Extensions'.
			'SUnitGroup'.
		}.

		spec group: 'SUnitGroup' with: {
			'System-Model'.
			'SUnit-Core'.
			'SUnit-Tests'.
			'Kernel-Tests'.
			'JenkinsTools-Core'}.

	]
]
