private
on: aStream 
	(stream := aStream) reset.
	stream binary
	"Note that 'reset' makes a file be text.  Must do this after."