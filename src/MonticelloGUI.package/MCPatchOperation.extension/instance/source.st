*MonticelloGUI
source
	"I am call on the changes/merge tools of Monticello if you disable the 'Use Polymorph difference tool' on your setting."

	^ self localChosen
		ifTrue: [ self fromSource ]
		ifFalse: [ self sourceText ]