private
constructPackagesSet: aPackageEnvironment
	| result |
	result := aPackageEnvironment packages collect:[:c | c name].
		^(result reject: [:each | each isNil]) asSet asSortedCollection: [:a :b | a < b ].