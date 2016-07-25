accessing
replaceFrom: start to: stop with: aText

	| txt |
	txt := aText asText.	"might be a string"
	string := string copyReplaceFrom: start to: stop with: txt string.
	runs := runs copyReplaceFrom: start to: stop with: txt runs