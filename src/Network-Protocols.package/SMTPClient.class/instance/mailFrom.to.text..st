public protocol
mailFrom: sender to: recipientList text: messageText
	"deliver this mail to a list of users.  NOTE: the recipient list should be a collection of simple internet style addresses -- no '<>' or '()' stuff"

	self mailFrom: sender.
	recipientList do: [ :recipient |
		self recipient: recipient ].
	self data: messageText.
