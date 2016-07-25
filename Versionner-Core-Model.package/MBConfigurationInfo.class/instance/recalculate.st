updating
recalculate
	branch := workingCopy := baselines := versions := nil.
	super recalculate.
	self configurationRoot announcer announce: (MBConfigurationInfoChanged changed: self).
