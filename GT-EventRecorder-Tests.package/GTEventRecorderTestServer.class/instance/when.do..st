events
when: anAnnouncement do: aBlock
	"Subscribe myself for events that belongs to my ZnServer."
	^ ZnLogEvent announcer 
			when: anAnnouncement 
			do: [ :announcement | announcement serverId = server route ifTrue: [ aBlock cull: announcement ] ]
			for: self.