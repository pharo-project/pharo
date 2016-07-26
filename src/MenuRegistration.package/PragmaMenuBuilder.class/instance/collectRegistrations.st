registrations handling
collectRegistrations
	"Retrieve all pragma methods and evaluate them by passing the 
	MenuRegistration class as argument. The result is a list of trees
	stored in my itemList inst var"

	| menu |
	menu := PragmaMenuAndShortcutRegistration model: self model.
	self pragmaCollector
		do: [ :prg | 
			self
				currentRoot: self
				while: [ prg methodClass theNonMetaClass
						perform: prg methodSelector
						with: menu ] ].
	self interpretRegistration: menu