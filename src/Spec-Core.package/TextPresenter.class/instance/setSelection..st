api
setSelection: anInterval
	"Set the text selection without changing the readSelectionBlock"
	"<api: #st getter: #getSelection>"

	selection ifNotNil: [: holder | holder value: anInterval ]