metacello
dependentPackages
	"Return the list of dependent packages for the last baseline"

	"This method must be rewritten. It is a shame..."
	
	| lastBaseLineName packageNames |
	lastBaseLineName := self lastBaseLine.

	packageNames := OrderedCollection new.
	(self configurationClass project version: lastBaseLineName)
		record loadDirective packageDirectivesDo: [:directive |
			packageNames add: directive file ].
		
	^ packageNames collect: [:p | 
		(p includes: $.)
			ifTrue: [ (p includes: $-)
						ifTrue: [ (p copyUpToLast: $-) ]
						ifFalse: [ (p copyUpTo: $.) ] ]
			ifFalse: [ p ]
		]

"	lastBaseLineMethod := (self configurationClass methods 
							select: [:cm | cm selector beginsWith: 'baseline'])
								select: [:cm | cm pragmas first arguments first = lastBaseLineName].
	lastBaseLineMethod := lastBaseLineMethod first.
"	
	"This does not work! No idea why!"
"	versionConstructor := (Smalltalk at: #MetacelloVersionConstructor) new.
	self configurationClass new perform: lastBaseLineMethod selector with: versionConstructor.
	versionSpec := (Smalltalk at: #MetacelloVersionSpec) new.
	versionConstructor root: versionSpec
"
	"This is probably the ugliest piece of code I ever wrote. There is really nothing to be proud of."
"	possiblePackageNames := 
		((lastBaseLineMethod literals select: [ :l | l class == ByteString ])
			reject: [:l | l beginsWith: 'http']).
	packageInfos := PackageInfo allPackages select: [ :pi | possiblePackageNames includes: pi packageName ].
"
	"packageInfos now contains the package that the lastest baseline depends on"
	"^ packageInfos collect: #packageName"

	