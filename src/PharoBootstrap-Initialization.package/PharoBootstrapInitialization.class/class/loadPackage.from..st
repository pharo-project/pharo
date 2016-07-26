loading
loadPackage: mcVersion from: aRepository
	mcVersion ifNotEmpty: [
		[ 
			UIManager default inform: 'Loading... ' , mcVersion.
			MCVersionLoader new
				addVersion: (aRepository versionFrom: mcVersion);
				load
		] on: Warning do: [ :warning | 
				UIManager default inform: 'Warning: ', warning messageText.
				warning resume.
		].
		UIManager default inform: 'Loaded ', mcVersion]
