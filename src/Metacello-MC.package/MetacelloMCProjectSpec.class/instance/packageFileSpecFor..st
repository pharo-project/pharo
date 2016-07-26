accessing
packageFileSpecFor: aMetacelloPackagesSpec

	^(aMetacelloPackagesSpec project projectReferenceSpec)
			name: self name;
			projectReference: self copy;
			yourself.