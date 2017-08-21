example
abstractExample
	| example |
	example := self new.
	example
		title: self name asString , ' example';
		label: 'I am a label'.
	^ example