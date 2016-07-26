registrations handling
collectRegistrations
	| menu |
	menu := PragmaMenuAndShortcutRegistration model: self.
	self pragmaCollector
		do: [ :prg | 
			prg methodClass theNonMetaClass
				perform: prg methodSelector
				with:
					(menu
						platform: prg arguments;
						yourself) ].
	self interpretRegistration: menu