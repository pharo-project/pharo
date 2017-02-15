accessing
clipboardText: text 

	| string data |
	string := text asString.
	self noteRecentClipping: text asText copyWithoutExternalReferences.
	contents := text asText copyWithoutExternalReferences.
	data := (string convertToWithConverter: UTF8TextConverter new) asByteArray.
	clipboard = 0 ifTrue: 
		[^super clipboardText: text].
	self clearClipboard.
	self primAddClipboardData: clipboard data: data dataFormat: 'public.utf8-plain-text'