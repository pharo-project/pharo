building
buildPackageSearch

	^ SearchMorph new
		model: self;
		updateSelector: #packageSearchUpdate:;
		ghostText: 'PackageA|PackageB';
		yourself