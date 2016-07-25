menus behavior
removePackages
	| names packages |
	
	packages := self selectedPackages. 
	packages ifEmpty: [ ^ self ].
	
	names := (packages 
		collect: [:package | package name ])
		joinUsing: String cr.

	(UIManager default 
		confirm: ('Are you sure you want to delete the selection(s) named ', String cr, names, ' and their classes ?'))
		ifTrue: [ packages do: [ :each | each asNautilusSelection removeTarget: self ].
			self updatePackageView ]