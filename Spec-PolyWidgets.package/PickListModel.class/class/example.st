example
example
	"self example"
	| instance |
	
	instance := self new.
	instance openWithSpec.
	instance items: { {1. 2. 4}. 'Foo'. 123}