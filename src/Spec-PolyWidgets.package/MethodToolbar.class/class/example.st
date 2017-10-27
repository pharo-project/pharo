example
example
	| example |
	example := MethodToolbar new.
	example
		method: ButtonPresenter >> #state:;
		method: nil;
		extent: 550 @ 100;
		openWithSpec.
		^example 