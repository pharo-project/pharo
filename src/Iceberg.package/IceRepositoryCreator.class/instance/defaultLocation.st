private accessing
defaultLocation
	| remoteToUse |
	remoteToUse := self remoteToUse.
	^ IceRepository repositoriesLocation 
		/ remoteToUse organizer 
		/ remoteToUse projectName