initialization
initializePresenter

	errorListModel whenSelectedItemChanged: [ :item | | header |
		item 
			ifNil: [ 
				vmVersion text: ''.
				imageVersion text: ''.
				timeStamp text: ''.
				]	
			ifNotNil: [
				header := FLMaterializer materializeHeaderFromFileNamed: item.
				vmVersion text: (header additionalObjectAt: #VM).
				imageVersion text: (header additionalObjectAt: #Image).
				timeStamp text: (header additionalObjectAt: #Timestamp)
				] 
	]