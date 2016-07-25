recording
stop
	recording := false.
	SystemAnnouncer uniqueInstance announce: (MetalinkChanged new link: link)