private
renameExtensionsPrefixedWith: oldName to: newName
	| protocols |
	
	protocols := self extensionMethods 
		collect: [ :each | each origin organization protocolNamed:  each protocol ]
		as: Set.
		
	protocols do: [ :each | each rename: '*', newName, (each name allButFirst: oldName size + 1) ]		
	