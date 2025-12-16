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
		spec package: 'Deprecation'.
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
		
		spec package: 'Transcript-NonInteractive'.
		spec package: 'ParseTreeRewriter'.
		spec package: 'PharoBootstrap-Initialization'.
		spec package: 'ReflectionMirrors-Primitives'.
		spec package: 'Shift-ClassBuilder'.
		spec package: 'System-Announcements'.
		spec package: 'System-CommandLineHandler'.
		spec package: 'Clap-Core'.
		spec package: 'Clap-Commands-Pharo' .
		spec package: 'System-Finalization'.
		spec package: 'System-Platforms'.
		spec package: 'System-SessionManager'.
		
		spec package: 'ClassDefinitionPrinters'.
		spec package: 'CodeExport'.
		spec package: 'System-Sources'.
		spec package: 'System-Support'.
		spec package: 'System-Version'.

		spec package: 'UIManager'.
		spec package: 'Zinc-Character-Encoding-Core'.
		
		spec package: 'FFI-Kernel'.
		
		spec package: 'CodeImport'.
		
		spec package: 'CodeImport-Commands'.
		
		spec package: 'OpalCompiler-Core'.
		spec package: 'DebugInfo'
			with: [ spec requires: #( 'OpalCompiler-Core' ) ].
		
		spec package: 'FileSystem-Core'.
		spec package: 'FileSystem-Disk'.

		spec package: 'System-NumberPrinting'.

		spec package: 'Random-Core'.
		spec package: 'SUnit-Core'.
		spec package: 'SUnit-Tests'.
		spec package: 'Kernel-Tests'.
		spec package: 'SUnit-Basic-CLI'.		
		BaselineOfMonticello corePackageNames do: [ :package | spec package: package ].

		spec group: 'KernelGroup' with: {
			'FFI-Kernel'.
			'Announcements-Core'.
			'Collections-Abstract'.
			'Collections-DoubleLinkedList'.
			'Collections-Native'.
			'Collections-Sequenceable'.
			'Collections-Streams'.
			'Collections-Strings'.
			'Collections-Support'.
			'Collections-Unordered'.
			'Collections-Weak'.
			'Files'.
			'Hermes'.
			'Kernel'.
			'Kernel-CodeModel'.
			'Kernel-BytecodeEncoders'.
			'Transcript-NonInteractive'.
			'PharoBootstrap-Initialization'.
			'Shift-ClassBuilder'.
			'System-Announcements'.
			'System-CommandLineHandler'.
			'System-Finalization'.
			'System-Platforms'.
			'System-SessionManager'.

			'ClassDefinitionPrinters'.
			'CodeExport'.
			'System-Sources'.
			
			'System-Support'.

			'Zinc-Character-Encoding-Core' } .

		"These packages are added using hermes after bootstrap"

		spec group: 'AdditionalPackages' with: {
			'Clap-Core'.
			'Clap-Commands-Pharo'.
			'ReflectionMirrors-Primitives'.
			'System-Time'.
			'Math-Operations-Extensions'.
			'System-NumberPrinting'.
			'System-Version'
		}.

		spec group: 'MultilingualGroup' with: {
			'Multilingual-Encodings' }.

		spec group: 'CompilerGroup' with: {
			'UIManager'.
			'AST-Core'.
			'Collections-Arithmetic'.
			'Collections-Atomic'.
			'Collections-Stack'.
			'CodeImport'.
			'CodeImport-Commands'.
			'DebugInfo'.
			'NumberParser'.
			'OpalCompiler-Core'.
			'ParseTreeRewriter'.
			'Deprecation'}.
		
		spec group: 'FileSystemGroup' with: {
			'FileSystem-Path'.
			'FileSystem-Core'.
			'FileSystem-Disk'}.
			
		spec group: 'MonticelloCore' with: BaselineOfMonticello corePackageNames.

		spec group: 'ToLoadByHermes' with: {
			'AdditionalPackages'.
			'MultilingualGroup'.
			'CompilerGroup'.
			'FileSystemGroup'.
			'Hermes-Extensions'.
			'MonticelloCore'.
			'SUnitGroup'.
		}.

		spec group: 'SUnitGroup' with: {
			'Deprecation'.
			'Random-Core'.
			'ParseTreeRewriter'.
			'SUnit-Core'.
			'SUnit-Tests'.
			'Kernel-Tests'.
			'SUnit-Basic-CLI'}.

	]
]
