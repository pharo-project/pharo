subclass creation
newAnonymousSubclass

	<localClassMethod> 
    "This method is supposed to be local in Class because of a good reason.
    We use this pragma to test if Class does not contain some accidental 
    local selectors."

	^ AnonymousClassInstaller make: [ :builder |
		builder
			superclass: self ;
			layoutClass: self classLayout class ].
