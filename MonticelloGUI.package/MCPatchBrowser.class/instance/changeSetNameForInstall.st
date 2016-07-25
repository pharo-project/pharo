actions
changeSetNameForInstall
	"Answer the name of the change set into which my selection will be installed.
	Derive this from my label.
	If I have no label, use the current change set."

	| tokens |
	label ifNil: [ ^ChangeSet current name ].
	tokens := label findTokens: ' '.
	tokens removeAllFoundIn: { 'changes'. 'between'. 'and' }.
	(tokens size = 3 and: [ tokens second = '<working' ]) ifTrue: [ ^tokens first, '-to-working' ].
	tokens size = 2 ifFalse: [ ^'InstalledPatches' ].
	^'{1}-to-{2}' format: tokens 