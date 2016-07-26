generate scripts/methods
writeCS: extensionAndNumber forUpdate: updateNumber withName: aSt
	"ScriptLoader new writeCS: '-md.2929' forUpdate: 7049 withName: 'cleanUpMethods'"
	
	(updateNumber asString, 'update', aSt, '.cs') asFileReference delete
		writeStreamDo: [ :stream |
			self generateCS: extensionAndNumber fromUpdate: updateNumber on: stream].
