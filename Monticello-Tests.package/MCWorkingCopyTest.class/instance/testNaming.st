tests
testNaming
	| repos version |

	repos := MCDictionaryRepository new.
	self assertNameWhenSavingTo: repos is: self packageName, '-abc.1'.
	self assertNameWhenSavingTo: repos is: self packageName, '-abc.2'.
	repos := MCDictionaryRepository new.
	self assertNameWhenSavingTo: repos is: self packageName, '-abc.3'.
	version := self snapshot.
	version info instVarNamed: 'name' put: 'foo-jf.32'.
	version load.
	self assertNameWhenSavingTo: repos is: 'foo-abc.33'.
	self assertNameWhenSavingTo: repos is: 'foo-abc.34'.
	version info instVarNamed: 'name' put: 'foo-abc.35'.
	repos storeVersion: version.
	self assertNameWhenSavingTo: repos is: 'foo-abc.36'.
	self assertNameWhenSavingTo: repos is: 'foo-abc.37'.
	version info instVarNamed: 'name' put: 'foo-abc.10'.
	repos storeVersion: version.
	self assertNameWhenSavingTo: repos is: 'foo-abc.38'.
	version info instVarNamed: 'name' put: 'foo2-ab.40'.
	version load.
	self assertNameWhenSavingTo: repos is: 'foo2-abc.41'.