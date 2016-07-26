comparing
<= aResolvedReference
	"Sort versions according to:

		1. package name
		2. branch name, list versions without branch last
		3. version number
		4. author name
		5. repository priority"
	
	self packageName = aResolvedReference packageName
		ifFalse: [ ^ self packageName <= aResolvedReference packageName ].
	self branch = aResolvedReference branch ifFalse: [ 
		^ (self branch isEmpty or: [ aResolvedReference branch isEmpty ])
			ifTrue: [ self branch size > aResolvedReference branch size ]
			ifFalse: [ self branch <= aResolvedReference branch ] ].
	self versionNumber = aResolvedReference versionNumber
		ifFalse: [ ^ self versionNumber <= aResolvedReference versionNumber ].
	self author = aResolvedReference author
		ifFalse: [ ^ self author <= aResolvedReference author ].
	self repository goferPriority = aResolvedReference repository goferPriority
		ifFalse: [ ^ self repository goferPriority <= aResolvedReference repository goferPriority ].
	^ true