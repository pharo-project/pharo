accessing
selectionActions
	"These are actions that make sense only in the context of values held in the selection port."
	^ selectionActions ifNil: [ selectionActions := OrderedCollection new ]