private
interpretRegistration: aRegistration
	
	aRegistration handOutItems do: [:item || node |
		node := KMKeymapBuilder 
					for: item shortcut
					platform: item platform.
		item help 
			ifNil: [
				node
					category: item category
					shortcut: item default
					do: item action ]
			ifNotNil:[ 
				node
					category: item category
					shortcut: item default
					do: item action
					description: item help ]]