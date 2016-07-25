example
example
	| widget |
	
	widget := self new.
	widget
		title: 'Selectors list';
		list: self selectors.

	widget openWithSpec.