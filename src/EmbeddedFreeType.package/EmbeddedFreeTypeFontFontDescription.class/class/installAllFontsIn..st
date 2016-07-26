accessing
installAllFontsIn: provider
	self allSubclasses do: [ :each | each installFontsIn: provider ]