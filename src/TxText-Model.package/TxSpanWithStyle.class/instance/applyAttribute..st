operation
applyAttribute: anAttribute
	"Apply attribute to whole receiver"
	style := self text styles extend: style with: anAttribute.
