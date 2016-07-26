preferences
omittedHeaderFields
	"Reply a list of fields to omit when displaying a nice simple message"

	"Note that heads of the form
		X-something: value
	are filtered programatically.  This is done since we don't want any of them
	and it is impossible to predict them in advance."

	^ #(
			'comments'
			'priority'
			'disposition-notification-to'
			'content-id'
			'received'
			'return-path'
			'newsgroups'
			'message-id'
			'path'
			'in-reply-to'
			'sender'
			'fonts'
			'mime-version'
			'status'
			'content-type'
			'content-transfer-encoding'
			'errors-to'
			'keywords'
			'references'
			'nntp-posting-host'
			'lines'
			'return-receipt-to'
			'precedence'
			'originator'
			'distribution'
			'content-disposition'
			'importance'
			'resent-to'
			'resent-cc'
			'resent-message-id'
			'resent-date'
			'resent-sender'
			'resent-from'
			'delivered-to'
			'user-agent'
			'content-class'
			'thread-topic'
			'thread-index'
			'list-help'
			'list-post'
			'list-subscribe'
			'list-id'
			'list-unsubscribe'
			'list-archive'
		)
