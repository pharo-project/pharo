"
I am a tool that visually represents an MCVersion. 

While I can be opened on anMCVersion, you've probably seen me most often after saving a package in the Monticello Browser. 

I have buttons to perform common version-related actions - like browsing, loading, and viewing changes. In my main text area, I display the following information about my version - name, author, timestamp, UUID, ancestors and log message.
"
Class {
	#name : #MCVersionInspector,
	#superclass : #MCTool,
	#instVars : [
		'version'
	],
	#category : 'MonticelloGUI'
}

{ #category : #icons }
MCVersionInspector class >> taskbarIconName [
	"Answer the icon for the receiver in a task bar."

	^ #smallSaveIcon
]

{ #category : #actions }
MCVersionInspector >> adopt [
	(self confirm:
'Modifying ancestry can be dangerous unless you know
what you are doing.  Are you sure you want to adopt
',self version info name, ' as an ancestor of your working copy?')
		ifTrue: [self version adopt]
]

{ #category : #actions }
MCVersionInspector >> browse [

	^ self version browse
]

{ #category : #'morphic ui' }
MCVersionInspector >> buttonSpecs [
       ^ #((Browse browse 'Browse this version' hasVersion)
               (History history 'Browse the history of this version' hasVersion)
               (Changes changes 'Browse the changes this version would make to the
image' hasVersion)
               (Load load 'Load this version into the image' hasVersion)
               (Merge merge 'Merge this version into the image' hasVersion)
               (Adopt adopt 'Adopt this version as an ancestor of your working copy'
hasVersion)
               (Copy save 'Copy this version to another repository' hasVersion)
               (Diff diff 'Create an equivalent version based on an earlier release'
hasVersion))
]

{ #category : #actions }
MCVersionInspector >> changes [
	"Open a patch morph for the changes."
	
	self
		viewChanges: self version changes
		from: self version workingCopy description
		to: self version info name
]

{ #category : #'morphic ui' }
MCVersionInspector >> defaultLabel [
	^ 'Version: ', self version info name
]

{ #category : #actions }
MCVersionInspector >> diff [
	| ancestorVersion |
	self pickAncestor ifNotNil:
		[:ancestor |
		ancestorVersion := self version workingCopy repositoryGroup versionWithInfo: ancestor.
		(self version asDiffAgainst: ancestorVersion) open]
]

{ #category : #testing }
MCVersionInspector >> hasVersion [
	^version notNil
]

{ #category : #actions }
MCVersionInspector >> history [
	(MCVersionHistoryBrowser new ancestry: self versionInfo) show
]

{ #category : #'morphic ui' }
MCVersionInspector >> initialExtent [
	^ 400@200
]

{ #category : #actions }
MCVersionInspector >> load [
	Cursor wait showWhile: [self version load]
]

{ #category : #actions }
MCVersionInspector >> merge [
	self version merge
]

{ #category : #'morphic ui' }
MCVersionInspector >> pickAncestor [
	| index versions |
	versions := self version info breadthFirstAncestors.
	index := UIManager default chooseFrom: (versions collect: [:ea | ea name])
				title: 'Ancestor:'.
	^ index = 0 ifFalse: [versions at: index]
]

{ #category : #'morphic ui' }
MCVersionInspector >> pickRepository [
	| index |
	index := UIManager default chooseFrom: (self repositories collect: [:ea | ea description])
				title: 'Repository:'.
	^ index = 0 ifFalse: [self repositories at: index]
]

{ #category : #'morphic ui' }
MCVersionInspector >> repositories [
	^ MCRepositoryGroup default repositories
]

{ #category : #actions }
MCVersionInspector >> save [
	
	self pickRepository 
		ifNotNil: [ :ea | 
			self version dependencies do: [ :each | ea storeVersion: each resolve ]. 
			ea storeVersion: self version ]
]

{ #category : #accessing }
MCVersionInspector >> summary [
	^self hasVersion
		ifTrue: [ self versionSummary ]
		ifFalse: [ String new ]
]

{ #category : #accessing }
MCVersionInspector >> version [
	^ version
]

{ #category : #accessing }
MCVersionInspector >> version: aVersion [
	version := aVersion
]

{ #category : #accessing }
MCVersionInspector >> versionInfo [
	^ self version info
]

{ #category : #accessing }
MCVersionInspector >> versionSummary [
	^ self version summary
]

{ #category : #'morphic ui' }
MCVersionInspector >> widgetSpecs [
	^ #(
		((buttonRow) (0 0 1 0) (0 0 0 30))
		((textMorph: summary) (0 0 1 1) (0 30 0 0))
		)
]
