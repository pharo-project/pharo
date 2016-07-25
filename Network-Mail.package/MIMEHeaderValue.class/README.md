I contain the value portion of a MIME-compatible header.

I must be only initialized with the value and not the field name.  E.g. in processing
	Subject: This is the subject
the MIMEHeaderValue should be given only 'This is the subject'

For traditional non-MIME headers, the complete value returned for mainValue and paramaters returns an empty collection.

For MIME headers, both mainValue and parameters are used.