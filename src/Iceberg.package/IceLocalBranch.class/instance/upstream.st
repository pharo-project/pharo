accessing
upstream
	^ upstream ifNil: [ 
		self backend upstream 
			ifNotNil: [ :upstreamName |
				upstream := (IceRemoteBranch named: upstreamName inRepository: self repository) ] ]