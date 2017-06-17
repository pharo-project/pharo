accessing remotes
removeRemote: aRemote
	self backend removeRemote: aRemote.
	self detachRemote: aRemote
