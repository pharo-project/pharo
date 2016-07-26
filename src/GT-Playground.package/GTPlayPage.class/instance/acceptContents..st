compatibility-tools
acceptContents: aStringOrtext

	self flag: #TODO. "Need support for what lies in the comment down here"
	"self fileName ifNotNil: [ self saveString: aStringOrtext toFile: self fileName ].

	acceptAction ifNotNil:[acceptAction value: aStringOrtext]."
	self saveContent: aStringOrtext.
	^ true