*opalcompiler-core
asPragma
	^ Pragma keyword: selector arguments: (arguments collect: [ :each | each value ]) asArray