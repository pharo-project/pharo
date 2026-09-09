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
BaselineOfPharoBootstrap class >> kernelPackageNames [

	^ self packagesOfGroupNamed: #KernelGroup
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
BaselineOfPharoBootstrap class >> packagesToExportAsMCZ [

	^ self packagesRecursiveIn: 'ToExportAsMCZ'
]

{ #category : 'accessing' }
BaselineOfPharoBootstrap class >> packagesToExportWithHermes [

	^ self packagesRecursiveIn: 'ToLoadByHermes'
]

{ #category : 'baselines' }
BaselineOfPharoBootstrap >> baseline: spec [

	<baseline>
	spec for: #common do: [
			BaselineOfKernel corePackageNames do: [ :package | spec package: package ].
			BaselineOfKernel additionalPackagesPackageNames do: [ :package | spec package: package ].
			BaselineOfCompiler corePackageNames do: [ :package | spec package: package ].
			BaselineOfFileSystem corePackageNames do: [ :package | spec package: package ].
			BaselineOfTraits corePackageNames do: [ :package | spec package: package ].
			BaselineOfMonticello corePackageNames do: [ :package | spec package: package ].
			BaselineOfMetacello corePackageNames do: [ :package | spec package: package ].

			spec group: 'KernelGroup' with: BaselineOfKernel corePackageNames.
			spec group: 'AdditionalPackages' with: BaselineOfKernel additionalPackagesPackageNames.
			spec group: 'CompilerGroup' with: BaselineOfCompiler corePackageNames.
			spec group: 'FileSystemGroup' with: BaselineOfFileSystem corePackageNames.
			spec group: 'TraitGroup' with: BaselineOfTraits corePackageNames.
			spec group: 'MonticelloCore' with: BaselineOfMonticello corePackageNames.
			spec group: 'MetacelloGroup' with: BaselineOfMetacello corePackageNames.

			"Group of packages to load with Hermes."
			spec group: 'ToLoadByHermes' with: { 'AdditionalPackages'. 'CompilerGroup'. 'FileSystemGroup'. 'TraitGroup'. 'MonticelloCore' . 'MetacelloGroup' }.

			"Group of packages to export as MCZ"
			spec group: 'ToExportAsMCZ' with: { 'KernelGroup'. 'ToLoadByHermes' } ]
]
