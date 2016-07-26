merging
mergeMap

	| map |
	map := super mergeMap.
	map at: #name put: name.
	map at: #className put: className.
	map at: #versionString put: versionString.
	map at: #operator put: operator.
	map at: #loads put: loads.
	map at: #preLoadDoIt put: preLoadDoIt.
	map at: #postLoadDoIt put: postLoadDoIt.
	^map