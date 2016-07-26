initialization
fromAnnouncement: anAnnouncement
	query := self textInfoFrom: anAnnouncement query.
	processors := (anAnnouncement processors flatCollect: [ :eachProcessor |
		Array 
			with: eachProcessor title
			with: eachProcessor allFilteredCandidates size ]) asArray