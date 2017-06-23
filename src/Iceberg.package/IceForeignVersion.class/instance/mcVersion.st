accessing
mcVersion
	^ mcVersion ifNil: [ mcVersion := self package workingCopy loadedVersion ]