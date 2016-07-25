copying
copyReplaceFrom: start to: stop with: aTextOrString

	| txt |
	txt := aTextOrString asText.	"might be a string"
	^self class 
             string: (string copyReplaceFrom: start to: stop with: txt string)
             runs: (runs copyReplaceFrom: start to: stop with: txt runs)
