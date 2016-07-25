actions
serializeAndSend: aReport

	"asyncronous upload to avoid UI hangups"
	[ self send: (self serialize: aReport) ] fork