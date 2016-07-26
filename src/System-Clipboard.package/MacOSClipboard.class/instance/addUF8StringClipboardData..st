private
addUF8StringClipboardData: aString
	| ba  |

	self clearClipboard.
	ba := aString convertToWithConverter: (UTF8TextConverter new).
	self addClipboardData: ba dataFormat: 'public.utf8-plain-text'
