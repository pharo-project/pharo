comparing
referencesSameRemoteLocationAs: another
	^ (self url withoutSuffix: '.git') = (another url withoutSuffix: '.git')