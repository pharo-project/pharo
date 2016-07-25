updating
updateUIBlock
	^ testAnnouncementBlock ifNil: [
		testAnnouncementBlock :=  [ :announcement | self updateStatus: true ] ].