*Polymorph-Widgets
passivateTopWindow
	"Make no window the top window."

	TopWindow ifNotNil: [ :window | 
		TopWindow := nil.
		window privateBePassive.
	]
