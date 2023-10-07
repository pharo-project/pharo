Extension { #name : 'SystemAnnouncer' }

{ #category : '*Deprecated12' }
SystemAnnouncer >> classReorganized: aClass [

	self deprecated: 'This announcement should not be raised anymore. Instead ProtocolAdded, ProtocolRemoved and ProtocolRenamed should be use.'.
	self announce: (ClassReorganized class: aClass)
]
