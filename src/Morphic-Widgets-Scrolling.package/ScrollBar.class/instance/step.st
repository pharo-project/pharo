stepping and presenter
step
	| action |
	action := self scrollBarAction.
	action ifNotNil:[self perform: action].