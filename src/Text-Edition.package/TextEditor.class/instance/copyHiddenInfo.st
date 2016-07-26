editing keys
copyHiddenInfo
	"In TextLinks, TextDoits, TextColor, and TextURLs, there is hidden
info.  Copy that to the clipboard.  You can paste it and see what it is.
Usually enclosed in <>."

	^ self clipboardTextPut: self hiddenInfo asText