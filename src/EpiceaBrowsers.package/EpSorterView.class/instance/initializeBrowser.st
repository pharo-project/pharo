initialization
initializeBrowser

	super initializeBrowser.

	browser itemsModel
		dragEnabled: false;
		dropEnabled: true;
		wantDropBlock: [ :transfer |
			"We have to filter since the drop could come from anywhere (for example, from Nautilus)"
			| answer |
			answer := true.
			transfer 
				valueOfProperty: #epiceaDraggedItems 
				ifAbsent: [ answer := false ].
			answer ];
		acceptDropBlock: [ :transfer :event :source :receiver | 
			self
				drop: (transfer valueOfProperty: #epiceaDraggedItems)
				from: (transfer valueOfProperty: #epiceaSourceView) ].
