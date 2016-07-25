Morphic-Base-Windows
findWindow: evt
	"Present a menu names of windows and naked morphs, and activate the one that gets chosen.  Collapsed windows appear below line, expand if chosen; naked morphs appear below second line; if any of them has been given an explicit name, that is what's shown, else the class-name of the morph shows; if a naked morph is chosen, bring it to front and have it don a halo."
	| menu expanded collapsed nakedMorphs |
	menu := UIManager default newMenuIn: self for: self.
	expanded := self  windowsSatisfying: [:w | w isCollapsed not].
	collapsed := self  windowsSatisfying: [:w | w isCollapsed].
	nakedMorphs := self submorphsSatisfying:[:m | m isSystemWindow not].
	(expanded isEmpty and: [collapsed isEmpty and: [nakedMorphs isEmpty]]) ifTrue: [^ self inform: 'No morph in world'].
	(expanded asSortedCollection: [:w1 :w2 | w1 label caseInsensitiveLessOrEqual: w2 label]) do:
		[:w | menu add: w label target: w selector: #activateAndForceLabelToShow.
			w model canDiscardEdits ifFalse: [menu lastItem color: Color red]].
	(expanded isEmpty or: [ collapsed isEmpty and: [ nakedMorphs isEmpty ]]) ifFalse: [menu addLine].
	(collapsed asSortedCollection: [:w1 :w2 | w1 label caseInsensitiveLessOrEqual: w2 label]) do: 
		[:w | menu add: w label target: w selector: #collapseOrExpand.
		w model canDiscardEdits ifFalse: [menu lastItem color: Color red]].
	nakedMorphs isEmpty ifFalse: [menu addLine].
	(nakedMorphs asSortedCollection: [:w1 :w2 | w1 class name caseInsensitiveLessOrEqual: w2 class name]) do:
		[:w | menu add: w class name target: w selector: #comeToFrontAndAddHalo].
	menu addTitle: 'find window'.
	menu popUpEvent: evt in: self.