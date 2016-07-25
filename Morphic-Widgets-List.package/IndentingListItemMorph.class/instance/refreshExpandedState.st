private-container protocol
refreshExpandedState

 	| newChildren toDelete c |
	toDelete := OrderedCollection new.
	firstChild ifNotNil: [
		firstChild withSiblingsDo: [ :aNode | aNode recursiveAddTo: toDelete].
	].
	container noteRemovalOfAll: toDelete.
	(isExpanded and: [complexContents hasContents]) ifFalse: [
		^self changed
	].
	(c := complexContents contents) isEmpty ifTrue: [^self changed].
	newChildren := container 
		addSubmorphsAfter: self 
		fromCollection: c 
		allowSorting: true.
	firstChild := newChildren first.
