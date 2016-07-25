tests
testVersion10

	| x y |
	self assert: ((x := (({
		self versionClass fromString: '1.0'.
		self versionClass fromString: '0.7'.
		self versionClass fromString: '0.8'.
		self versionClass fromString: '0.9'.
		self versionClass fromString: '1.0.1'
	} sort: [:a :b | a <= b ]) collect: [:each | each versionString ]) asArray) = (y := #( '0.7' '0.8' '0.9' '1.0' '1.0.1')))