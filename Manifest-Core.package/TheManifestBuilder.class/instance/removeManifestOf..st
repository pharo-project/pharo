adding/removing
removeManifestOf: aItem
	
	(self manifestOf: aItem) ifNotNil: [ :myManifest | 
			self removeClass: myManifest  ]