accessing
actions
	"These are actions that make sense for the entire presentation."
	actions ifNil: [actions := OrderedCollection new].
	^ actions