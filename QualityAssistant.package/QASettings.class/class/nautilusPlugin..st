settings-accessing
nautilusPlugin: aBoolean

	aBoolean
		ifTrue:  [ QANautilusPlugin install ]
		ifFalse: [ QANautilusPlugin uninstall ]