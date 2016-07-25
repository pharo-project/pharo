accessing
clipboardText: text 

	| string data |
	string := text asString.
	self noteRecentClipping: text asText.
	contents := text asText.
	data := (string convertToWithConverter: UTF8TextConverter new) asByteArray.
	clipboard = 0 ifTrue: 
		[^super clipboardText: text].
	self clearClipboard.
	self primAddClipboardData: clipboard data: data dataFormat: 'public.utf8-plain-text'