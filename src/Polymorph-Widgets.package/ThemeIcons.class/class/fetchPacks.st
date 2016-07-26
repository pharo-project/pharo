accessing
fetchPacks
	"Retrieve all packs available from repository"
	| jsonList |
	
	[ jsonList := ZnClient new 
		get: 'https://api.github.com/repos/pharo-project/pharo-icon-packs/branches';
		contents ] 
	on: Error do: [ :e | 
		"in case of error (no inet connection, for example) I retrieve just current icon set"
		e crLog.
		^ { self current } ].
	^ (STON fromString: jsonList) 
		collect: [ :each | | packName |
			"Ensure we have just one instance of current pack"
			packName := each at: 'name'.
			packName = self current name
				ifTrue: [ self current ]
				ifFalse: [ self named: packName ] ]
		as: Array