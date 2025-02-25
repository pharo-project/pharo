"
I am a command to commit given packages.

I use Iceberg if it is repository of package. 
Otherwise I use Komitter.
"
Class {
	#name : 'ClyCommitPackageCommand',
	#superclass : 'SycPackageCommand',
	#category : 'Calypso-SystemPlugins-Monticello-Browser',
	#package : 'Calypso-SystemPlugins-Monticello-Browser'
}

{ #category : 'testing' }
ClyCommitPackageCommand class >> canBeExecutedInContext: aToolContext [
	"if no packages are selected then command will commit all changes in image"
	^true
]

{ #category : 'activation' }
ClyCommitPackageCommand class >> fullBrowserMenuActivation [
	<classAnnotation>

	^CmdContextMenuActivation byRootGroupItemOrder: 10000 for: ClyFullBrowserPackageContext
]

{ #category : 'activation' }
ClyCommitPackageCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>

	^CmdShortcutActivation by: $s meta for: ClyFullBrowserGlobalContext
]

{ #category : 'accessing' }
ClyCommitPackageCommand >> defaultMenuIconName [
	^#monticelloPackage
]

{ #category : 'accessing' }
ClyCommitPackageCommand >> defaultMenuItemName [
	^'Commit'
]

{ #category : 'accessing' }
ClyCommitPackageCommand >> description [

^ 'I am a command to commit given packages' 
]

{ #category : 'execution' }
ClyCommitPackageCommand >> execute [

	| repoBrowser commitBrowser repos targetRepo |
	repoBrowser := self class environment
		               at: #IceTipRepositoriesBrowser
		               ifAbsent: [
		               ^ self inform: 'Iceberg 0.7 and higher is required' ].
	commitBrowser := self class environment
		                 at: #IceTipCommitBrowser
		                 ifAbsent: [
		                 ^ self inform: 'Iceberg 0.7 and higher is required' ].
	packages ifEmpty: [ ^ repoBrowser new open ].
	repos := IceRepository registry select: [ :repo |
		         packages anySatisfy: [ :package |
			         repo includesPackageNamed: package name ] ].
	repos ifEmpty: [
		^ self inform: 'Selected packages are not managed by Iceberg' ].
	targetRepo := repos size = 1
		              ifTrue: [ repos first ]
		              ifFalse: [
			              self morphicUIManager
				              chooseFrom: (repos collect: [ :each | each name ])
				              values: repos
				              title: 'Choose repository' ].
	targetRepo ifNil: [ ^ self ].
	(targetRepo isMissing or: [
		 targetRepo isCodeMissing or: [
			 targetRepo workingCopy isDetached or: [
				 targetRepo head isDetached or: [
					 targetRepo workingCopy project isUnborn ] ] ] ]) ifTrue: [
		self morphicUIManager
			alert:
				'The ' , targetRepo name
				, ' repository is not operational to commit.
Causes can be:
- The local repository is missing
- The code directory is missing
- The project is in detached head state
- Metadatas are missing.

You can fix this problem using the Iceberg tool.'
			title: 'Error'.
		^ self ].
	(commitBrowser onRepository: targetRepo) open
]
