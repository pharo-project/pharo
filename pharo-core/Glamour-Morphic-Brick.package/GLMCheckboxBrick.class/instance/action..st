checkbox-accessing
action: aBlockClosure 
	"aBlockClosure can receive as the first argument aBoolean, as the second argument this object."
	self 
		when: #onChecked 
			do: [ :anAnnouncement :anAnnouncer | 
					aBlockClosure cull: true cull: self cull: anAnnouncement cull: anAnnouncer ];
		when: #onUnchecked
			do: [ :anAnnouncement :anAnnouncer | 
					aBlockClosure cull: false cull: self cull: anAnnouncement cull: anAnnouncer ]