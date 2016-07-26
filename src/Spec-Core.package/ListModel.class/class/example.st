example
example
	| instance |

	instance := self new.
	instance openWithSpec.
	instance items: (Smalltalk allClasses).