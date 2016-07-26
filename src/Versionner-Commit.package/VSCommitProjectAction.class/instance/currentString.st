*Versionner-Commit-UI
currentString
	^ [ self project stableVersion ifNotNil: [  :version | version versionString  ] ]
	on: Error 
	do: [ :e | '' ]