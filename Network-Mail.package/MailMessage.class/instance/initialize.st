initialization
initialize
	"initialize as an empty message"

	super initialize.
	text := String cr.
	fields := Dictionary new.
	body := MIMEDocument contentType: 'text/plain' content: String cr