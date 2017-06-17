accessing
savedPackages
	^ [self repository savedPackages collect: [ :package | IcePackageModel modelFor: package ]]
		on: Error do: [ :error | 
			self inform: ('Error computing saved packages for repository: {1}. {2}'
				format: { self repository name. error }).
			#()
		]