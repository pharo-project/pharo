accessing
contents

	| ret state |
	state := converter saveStateOf: self.
	ret := self upToEnd.
	converter restoreStateOf: self with: state.
	^ ret.
