reading
versionFromFile: fileName
	^ self file: fileName streamDo: [:stream | self versionFromStream: stream]