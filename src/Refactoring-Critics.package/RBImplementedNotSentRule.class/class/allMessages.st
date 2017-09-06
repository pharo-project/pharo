accessing
allMessages
	"return all 'message sends' in the systed and cache them"

	allMessages ifNil: [
		allMessages := IdentitySet new.
	
		ProtoObject
			withAllSubclassesDo: [ :class | 
				class methodsDo: [ :method |
					self forApproxMessagesOf: method do: [ :mes |
						allMessages add: mes ] ].
				class class methodsDo: [ :method |
					self forApproxMessagesOf: method do: [ :mes |
						allMessages add: mes ] ] ] ].
			
	^ allMessages