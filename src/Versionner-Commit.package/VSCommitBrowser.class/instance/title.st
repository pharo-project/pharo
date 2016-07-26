accessing
title
	^ '{1} commit for {2}' format: { self commitVersionKind. self configurationName }