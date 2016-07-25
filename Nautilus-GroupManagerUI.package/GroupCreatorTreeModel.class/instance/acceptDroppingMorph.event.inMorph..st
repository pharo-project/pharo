drag and drop
acceptDroppingMorph: transferMorph event: evt inMorph: listMorph
	"transferMorph passenger complexContents item pour récupérer le package"

	| group package target |
	(listMorph scrollerSubMorphFromPoint: evt position) complexContents
		ifNil: [ 
			self model groupManager patternModel acceptEditsInView.
			^ self ]
		ifNotNil: [ :node | target := node ].
	target isGroup
		ifTrue: [ group := target item ]
		ifFalse: [ group := target parentNode item ].
	package := transferMorph passenger complexContents item.
	self model groupManager add: {package} into: group.
	self model groupTree updateList.
	self model groupManager patternModel acceptEditsInView