menu messages
sendersOfIt
	"Open a senders browser on the selected selector"

	| selectedSelector syst |
	self lineSelectAndEmptyCheck: [^ self].
	selectedSelector  := self selectedSelector.
	selectedSelector isNil ifTrue: [^ morph flash].
	syst := self model systemNavigation.
	syst browseAllSendersOrUsersOf: selectedSelector