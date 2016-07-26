menu messages
sendersOfIt
	"Open a senders browser on the selected selector"

	| selectedSelector syst |
	self lineSelectAndEmptyCheck: [ ^ self ].
	selectedSelector := self selectedSelector.
	selectedSelector isNil
		ifTrue: [ ^ textArea flash ].
	syst := self model interactionModel systemNavigation.
	syst browseAllSendersOrUsersOf: selectedSelector