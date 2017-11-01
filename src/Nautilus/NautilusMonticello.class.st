"
A NautilusMonticello is a class used to handle monticello integration into Nautilus by creating the menu entries
"
Class {
	#name : #NautilusMonticello,
	#superclass : #Model,
	#classVars : [
		'Default'
	],
	#category : 'Nautilus'
}

{ #category : #singleton }
NautilusMonticello class >> default [

	^ Default ifNil: [ Default := self basicNew initialize ]
]

{ #category : #'instance creation' }
NautilusMonticello class >> new [

	^ self default
]

{ #category : #'monticello menus' }
NautilusMonticello class >> packagesChangesMenu: aBuilder [
	<nautilusGlobalPackageMenu>
	| package mcpackage target |
	target := aBuilder model.
	(package := target selectedPackage) ifNil: [ ^ target ].
	mcpackage := package mcPackage.
	mcpackage ifNil: [ ^ self ].
	mcpackage isDirty
		ifTrue: [ (aBuilder item: #('Changes with ' #, #package #name))
				label: 'Changes with...';
				order: 1295;
				help: 'Changes';
				iconName: #monticelloPackageIcon;
				enabledBlock: [ target selectedPackages size < 2 ].
			mcpackage workingCopy repositoryGroup repositories
				doWithIndex: [ :repo :index | 
					(aBuilder item: 'Repo' , index printString)
						label: repo description;
						parent: #('Changes with ' #, #package #name);
						order: index;
						action: [ self default viewChangesFor: mcpackage in: repo ] ] ]
]

{ #category : #'monticello menus' }
NautilusMonticello class >> packagesCommitsMenu: aBuilder [
	<nautilusGlobalPackageMenu>
	| package mcpackage target |
	target := aBuilder model.
	(package := target selectedPackage) ifNil: [ ^ target ].
	mcpackage := package mcPackage.
	mcpackage ifNil: [ ^ self ].
	mcpackage isDirty
		ifTrue: [ | last group |
			(aBuilder item: #('Commit in ' #, #package #name))
				label: 'Commit in...';
				order: 1296;
				help: 'Commit';
				iconName: #monticelloPackageIcon;
				enabledBlock: [ target selectedPackages size < 2 ].
			(group := mcpackage workingCopy repositoryGroup) repositories
				doWithIndex: [ :repo :index | 
					last := (aBuilder item: 'Repo' , index printString)
						label: repo description;
						parent: #('Commit in ' #, #package #name);
						order: index;
						action: [ self default commit: mcpackage workingCopy in: repo ] ].
			last withSeparatorAfter.
			(aBuilder item: 'Add a repository')
				parent: #('Commit in ' #, #package #name);
				order: 99;
				action: [ self default addRepositoryTo: group ] ]
]

{ #category : #'monticello menus' }
NautilusMonticello class >> packagesCreateMenu: aBuilder [
	<nautilusGlobalPackageMenu>
	| package mcpackage target |
	target := aBuilder model.
	(package := target selectedPackage) ifNil: [ ^ target ].
	mcpackage := package mcPackage.
	mcpackage ifNotNil: [ ^ self ].
	(aBuilder item: #'Create a MC package')
		order: 1297;
		help: 'Create a MC package corresponding to my name';
		iconName: #monticelloPackageIcon;
		action: [ self default createMCPackageFor: target selectedPackage ];
		enabledBlock: [ target selectedPackages size < 2 ]
]

{ #category : #'monticello menus' }
NautilusMonticello class >> packagesOpenMenu: aBuilder [
	<nautilusGlobalPackageMenu>
	| package mcpackage target last group |
	target := aBuilder model.
	(package := target selectedPackage) ifNil: [ ^ target ].
	mcpackage := package mcPackage.
	mcpackage ifNil: [ ^ self ].
	(aBuilder item: #('Open ' #, #package #name))
		label: 'Open...';
		order: 1297;
		help: 'Open';
		iconName: #monticelloPackageIcon;
		enabledBlock: [ target selectedPackages size < 2 ].
	(group := mcpackage workingCopy repositoryGroup) repositories
		doWithIndex: [ :repo :index | 
			last := (aBuilder item: 'Repo' , index printString)
				label: repo description;
				parent: #('Open ' #, #package #name);
				order: index;
				action: [ self default open: mcpackage in: repo ] ].
	last ifNotNil: [ :lastEntry | lastEntry withSeparatorAfter ].
	(aBuilder item: 'Add a repository')
		parent: #('Open ' #, #package #name);
		order: 99;
		action: [ self default addRepositoryTo: group ]
]

{ #category : #'monticello menus behavior' }
NautilusMonticello >> addRepositoryTo: aGroup [
	| repo |
	(repo := MCTool newRepository) ifNil: [ ^ self ].
	aGroup addRepository: repo.
]

{ #category : #'monticello menus behavior' }
NautilusMonticello >> commit: workingCopy in: aRepository [
	| newVersion |

	newVersion := workingCopy newVersionIn: aRepository.
	newVersion ifNil: [ ^ self ].
	Cursor wait showWhile: [
		[ 	aRepository storeVersion: newVersion.
			aRepository storeDependencies: newVersion ] 
		ensure: [ (MCVersionInspector new version: newVersion) show ]]
]

{ #category : #'monticello menus behavior' }
NautilusMonticello >> createMCPackageFor: aPackage [
	| name |
	name := aPackage name.
	name isEmptyOrNil ifFalse: [ MCWorkingCopy forPackage: (MCPackage new name: name) ]
]

{ #category : #'monticello menus behavior' }
NautilusMonticello >> open: mcpackage in: repo [ 
	| workingCopy |

	repo ifNil: [ ^ self ].
	workingCopy := mcpackage workingCopy.
	repo morphicOpen: workingCopy
]

{ #category : #display }
NautilusMonticello >> viewChanges: patch from: fromDescription to: toDescription in: workingCopy [
	"Open a browser on the given patch."

	PSMCPatchMorph usedByDefault
		ifTrue: [((PSMCPatchMorph forPatch: patch)
						fromDescription: fromDescription;
						toDescription: toDescription;
						newWindow)
					title: ('Changes to {1}' translated format: {fromDescription});
					open]
		ifFalse: [(MCPatchBrowser forPatch: patch)
					label: 'Patch Browser: ', workingCopy description;
					show]
]

{ #category : #'monticello menus behavior' }
NautilusMonticello >> viewChangesFor: mcpackage in: repo [
	| workingCopy patch |

	workingCopy := mcpackage workingCopy.
	'Finding changes' displayProgressFrom: 0 to: 10 during:[:bar|
		bar current: 1.
		patch := workingCopy changesRelativeToRepository: repo.
		bar current: 3.
		patch isEmpty
			ifTrue: [
				workingCopy modified: false.
				bar current: 10.
				self inform: 'No changes' ]
			ifFalse: [
				workingCopy modified: true.
				bar current: 5.
				self
					viewChanges: patch
					from: workingCopy  packageName, ' (', workingCopy ancestry ancestorString, ')'
					to: ('Modified {1}' translated format: {workingCopy description})
					in: workingCopy ]]
]
