example
example
	| instance |
	
	instance := self new.
	instance extent: 300@25.
	instance openWithSpec.
	instance ghostText: 'This is a ghost'.
	instance text: 'This is a text'.
	