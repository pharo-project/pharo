tests
testMultiPartAlternative
	| m txt html  part1 part2 |
	
	txt := 'This is plain text.'.
	html :=  '<html><head></head><body>This is html.</body></html>'.
	
	m := MailMessage empty.
	m addAlternativePart: txt contentType: 'text/plain'. 
	m addAlternativePart: html contentType: 'text/html'.

	self assert: (((m fields at: 'content-type') at: 1) mainValue asLowercase = 'multipart/alternative'). 
	self assert: (m parts size = 2).

	part1 := m parts at: 1.
	part2 := m parts at: 2.

	self assert: (((part1 fields at: 'content-type') at: 1) mainValue asLowercase = 'text/plain').
	self assert: ((part1 body content) = txt).
	
	self assert: (((part2 fields at: 'content-type') at: 1) mainValue asLowercase = 'text/html').
	self assert: ((part2 body content) = html).
