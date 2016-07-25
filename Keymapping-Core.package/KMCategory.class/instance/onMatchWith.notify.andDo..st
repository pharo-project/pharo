matching
onMatchWith: anEventBuffer notify: aMatchListener andDo: aBlock
	self keymaps do: [ :entry | 
		entry onMatchWith: anEventBuffer 
			notify: aMatchListener 
			andDo: aBlock ].