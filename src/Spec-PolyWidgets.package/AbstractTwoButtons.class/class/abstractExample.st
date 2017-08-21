example
abstractExample
	| example |
	example := self new.
	example
		title: self name asString , ' example';
		extent: 100 @ 100.
	^ example