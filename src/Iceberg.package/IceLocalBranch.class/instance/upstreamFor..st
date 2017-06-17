private
upstreamFor: aRemote
	^ aRemote 
		upstreamForBranch: self
		backend: self backend