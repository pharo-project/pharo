package
isValidPackageName: name

	^ (name isNil or: [ name isEmpty or: [ name first = $* ]]) not