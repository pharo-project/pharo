"
I am a command to commit given packages.

I use Iceberg if it is repository of package. 
Otherwise I use Komitter.
"
Class {
	#name : #ClyCommitMCPackageCommand,
	#superclass : #SycPackageCommand,
	#category : #'Calypso-SystemPlugins-Monticello-Browser'
}

{ #category : #testing }
ClyCommitMCPackageCommand class >> canBeExecutedInContext: aToolContext [
	"if no packages are selected then command will commit all changes in image"
	^true
]

{ #category : #activation }
ClyCommitMCPackageCommand class >> fullBrowserMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemOrder: 10000 for: ClyPackageContextOfFullBrowser 
]

{ #category : #activation }
ClyCommitMCPackageCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation by: $s meta for: ClyGlobalContextOfFullBrowser 
]

{ #category : #accessing }
ClyCommitMCPackageCommand >> defaultMenuIconName [
	^#monticelloPackage
]

{ #category : #accessing }
ClyCommitMCPackageCommand >> defaultMenuItemName [
	^'Commit'
]

{ #category : #execution }
ClyCommitMCPackageCommand >> execute [
	| repoBrowser commitBrowser repos targetRepo |
	repoBrowser := self class environment at: #IceTipRepositoriesBrowser ifAbsent: [ ^ self inform: 'Iceberg 0.7 and higher is required' ].
	commitBrowser := self class environment at: #IceTipCommitBrowser ifAbsent: [ ^ self inform: 'Iceberg 0.7 and higher is required' ].
	packages ifEmpty: [ ^ repoBrowser new openWithSpec ].
	repos := IceRepository registry select: [ :repo | packages anySatisfy: [ :each | repo includesPackageNamed: each name ] ].
	repos ifEmpty: [ ^ self inform: 'Selected packages are not managed by Iceberg' ].
	targetRepo := repos size = 1 ifTrue: [ repos first ] ifFalse: [ UIManager default chooseFrom: (repos collect: #name) values: repos title: 'Choose repository' ].
	targetRepo ifNil: [ ^ self ].
	(targetRepo isMissing or: [ targetRepo isCodeMissing or: [ targetRepo isDetached or: [ targetRepo hasUnbornProject ] ] ])
		ifTrue: [ UIManager default
				alert:
					'The ' , targetRepo name
						,
							' repository is not operational to commit.
Causes can be:
- The local repository is missing
- The code directory is missing
- The project is in detached head state
- Metadatas are missing.

You can fix this problem using the Iceberg tool.'
				title: 'Error'.
			^ self ].
	(commitBrowser onRepository: targetRepo) openWithSpec
]
