menu - change set
openChangeSetBrowser
	| cs |
	
	cs := self selectedChangeSet ifNil: [^self].
	^Smalltalk tools browser openInEnvironment: (RBBrowserEnvironment new forClasses: cs changedClasses)