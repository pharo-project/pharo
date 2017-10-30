initialization
shortcutsRegistrationFrom: src to: dest
	
	src changesPresenterOn: $c meta do: [ self copyAllFrom: src to: dest ].
	src changesPresenterOn: $- meta do: [ self subtractFrom: src to: dest ]. 