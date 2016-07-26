api
onWindowClosed

	cache cacheChange 
		ifTrue: [	
			(UIManager default confirm: 'Do you want log all wrong violations in the Manifests
before closing the Critics Browser ?')
				ifTrue: [ cache logInManifest ]].
	
