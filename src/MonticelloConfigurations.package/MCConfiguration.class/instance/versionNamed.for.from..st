private
versionNamed: verName for: aDependency from: repo

	| baseName fileName ver |
	(repo filterFileNames: repo cachedFileNames forVersionNamed: verName) ifNotEmpty: [:cachedNames |
		fileName := cachedNames anyOne.
		ProgressNotification signal: '' extra: 'Using cached ', fileName.
		ver := repo versionFromFileNamed: fileName].
	ver ifNil: [
		baseName := self diffBaseFor: aDependency.
		(baseName notNil and: [baseName ~= verName and: [repo includesVersionNamed: baseName]]) ifTrue: [
			fileName := (MCDiffyVersion nameForVer: verName base: baseName), '.mcd'.
			ProgressNotification signal: '' extra: 'Downloading ', fileName.
			ver := repo versionFromFileNamed: fileName]].
	ver ifNil: [
		fileName := verName, '.mcz'.
		ProgressNotification signal: '' extra: 'Downloading ', fileName.
		ver := repo versionFromFileNamed: fileName].
	^ver