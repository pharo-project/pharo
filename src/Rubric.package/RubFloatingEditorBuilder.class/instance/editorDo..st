accessing
editorDo: aBlock
	editor ifNil: [ ^ self ].
	aBlock cull: editor