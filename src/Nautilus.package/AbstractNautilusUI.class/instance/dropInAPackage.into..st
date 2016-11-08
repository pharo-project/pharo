drag and drop
dropInAPackage: aCollection into: receiver
	aCollection
		do: [ :aClassOrAMethod | aClassOrAMethod dropIn: receiver forNautilusUI: self ]