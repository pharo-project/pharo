private
invokeMenu: evt
	"Invoke my menu in response to the given event."
	| menu |
	menu := self getMenu: evt shiftPressed.
	menu ifNotNil: [menu popUpEvent: evt in: self world]