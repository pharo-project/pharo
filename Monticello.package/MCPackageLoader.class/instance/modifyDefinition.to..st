patch ops
modifyDefinition: old to: new
	self addDefinition: new.
	obsoletions at: new put: old.