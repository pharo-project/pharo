initialization
shortcutsRegistrationFrom: src to: dest
	
	src changesModelOn: $c meta do: [ self copyAllFrom: src to: dest ].
	src changesModelOn: $- meta do: [ self subtractFrom: src to: dest ]. 