expression types
temporaryBlockVariablesFor: aBlockNode
	"Scan and answer temporary block variables."

	| variables |
	(self match: #verticalBar) ifFalse:
		"There are't any temporary variables."
		[^#()].

	variables := OrderedCollection new.
	[hereType == #word] whileTrue:
		[variables addLast: (encoder bindBlockTemp: self advance within: aBlockNode)].
	^(self match: #verticalBar)
		ifTrue: [variables]
		ifFalse: [self expected: 'Vertical bar']