private
methodGroupAsNode
	^ self model model class methodGroupManager groups
		collect: [ :each | PackageTreeMethodGroupNodeModel with: each model: self ]