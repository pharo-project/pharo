accessing
peekFor: item 

	| next state |
	"self atEnd ifTrue: [^ false]. -- SFStream will give nil"
	state := converter saveStateOf: self.
	(next := self next) == nil ifTrue: [^ false].
	item = next ifTrue: [^ true].
	converter restoreStateOf: self with: state.
	^ false.
