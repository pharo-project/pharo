subclass creation
newAnonymousSubclass
	^ AnonymousClassInstaller make: [ :builder |
		builder
			superclass: self ;
			layoutClass: self classLayout class ].
