example
example
	| example |
	example := MethodToolbar new.
	example
		method: ButtonModel >> #state:;
		method: nil;
		extent: 550 @ 100;
		openWithSpec.
		^example 