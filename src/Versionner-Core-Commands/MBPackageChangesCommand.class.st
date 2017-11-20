"
Command to browse package changes in versionner
"
Class {
	#name : #MBPackageChangesCommand,
	#superclass : #MBAbstractPackageCommand,
	#category : #Versionner-Core-Commands
}

{ #category : #private }
MBPackageChangesCommand >> changesForSymbolicVersion: symbolicVersion workingCopy: wc [
	| repository info ancestorVersion ref gofer resolved version packageSpec packageName ancestorVersionInfo |
	(repository := self selectRepositoryFromWorkingCopy: wc) ifNil: [ ^ self ].
	info := target configurationInfo.
	version := info project version: symbolicVersion.
	packageSpec := version
		packageNamed: wc packageName
		ifAbsent: [ ^ self uiManager inform: 'No package to compare ', target name, ' to in version' , symbolicVersion printString ].
	packageName := packageSpec file.
	ref := ((gofer := Gofer new)
		repository: repository;
		version: packageName) references first.
	(resolved := ref resolveAllWith: gofer) isEmpty
		ifTrue: [ 
			ref := ((gofer := Gofer new)
				repository: repository;
				package: packageName) references first.
			resolved := ref resolveAllWith: gofer ].
	ancestorVersionInfo := repository versionInfoFromVersionNamed: resolved last name.
	ancestorVersion := repository versionWithInfo: ancestorVersionInfo.
	self
		findChangesBetween: wc package snapshot
		toLabel: wc ancestry ancestorString , ' (' , info currentVersion versionString , ')'
		and: ancestorVersion snapshot
		fromLabel: ancestorVersion info name , ' (' , symbolicVersion printString , ' )'
]

{ #category : #documentation }
MBPackageChangesCommand >> documentation [
	^ 'View changes to the package ancestor in the selected repository or the corresponding package version in the selected symbolic version.'
]

{ #category : #execute }
MBPackageChangesCommand >> execute [
	"View the changes made in the working copy."

	| wc repositoryOrSymbolicVersion |
	wc := target spec workingCopy ifNil: [ ^ self ].
	wc ancestors isEmpty
		ifTrue: [ ^ self ].
	(repositoryOrSymbolicVersion := self selectRepositoryOrSymbolicVersion: wc) ifNil: [ ^ self ].
	repositoryOrSymbolicVersion isSymbol
		ifTrue: [ self changesForSymbolicVersion: repositoryOrSymbolicVersion workingCopy: wc]
		ifFalse: [ self findChangesFor: wc relativeToRepository: repositoryOrSymbolicVersion ]
]

{ #category : #utility }
MBPackageChangesCommand >> selectRepositoryOrSymbolicVersion: workingCopy [
	| project symbolicVersions repos list values |
	project := target spec project.
	symbolicVersions := project symbolicVersionSymbols select: [ :each | project hasVersion: each ].
	repos := workingCopy repositoryGroup repositories.
	list := (symbolicVersions collect: [:each | each printString]), (repos collect: [ :r | r description ]).
	values := symbolicVersions, repos.
	^self uiManager
		chooseFrom: list
		values: values
		lines: {symbolicVersions  size}
		title: 'Please choose which symbolic version or repository'.

]

{ #category : #accessing }
MBPackageChangesCommand >> title [
	^ 'changes'
]
