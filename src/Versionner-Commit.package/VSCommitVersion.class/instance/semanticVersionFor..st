private
semanticVersionFor: aProject
	| projectLatestVersion |
	
	projectLatestVersion := self latestVersion: (aProject configurationClass project).
	^ self class
		parseSemanticVersionFor: projectLatestVersion
		ifError: [ 
			self crLog: 'Something went wrong trying to parse ', (projectLatestVersion asString).
			#(0 0 0) ]