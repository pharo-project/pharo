generate scripts/methods
writeCS: extensionAndNumber forUpdate: updateNumber
	"ScriptLoader new writeCS: '-sd.210' forUpdate: 7037"
	
	self writeCS: extensionAndNumber forUpdate: updateNumber withName: 'changeMe'