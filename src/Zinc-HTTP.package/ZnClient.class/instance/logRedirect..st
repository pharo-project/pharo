private logging
logRedirect: url
	^ (self newLogEvent: ZnClientFollowingRedirectEvent)
		target: url;
		emit