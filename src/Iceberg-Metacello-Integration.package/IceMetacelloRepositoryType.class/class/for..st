instance creation
for: aLocationString
	^ (self allSubclasses
		detect: [ :each | each isSuitableForLocation: aLocationString ])
		location: aLocationString