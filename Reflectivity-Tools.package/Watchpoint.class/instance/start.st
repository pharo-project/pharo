recording
start
	recording := true.
	SystemAnnouncer uniqueInstance announce: (MetalinkChanged new link: link)