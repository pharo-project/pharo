accessing
message
	<todo: 'use encoding to properly read the message'>
	|  encoding |
	encoding := self commit_message_encoding: self.
	^ self commit_message: self