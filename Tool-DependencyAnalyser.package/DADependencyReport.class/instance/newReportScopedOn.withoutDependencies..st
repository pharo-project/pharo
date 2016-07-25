converting
newReportScopedOn: aPackageName withoutDependencies: aListOfPackageNameToIgnore
	"Generates a new dependency report discarding 
	- all packages that are not present in aPackageName transitive dependencies
	- all packages in aListOfPackageNameToIgnore"
	| report dependenciesToInclude |
	report := self class new.
	report
		imageVersion: imageVersion; 
		analysisRunString: analysisRunString , ' scoped on ' , aPackageName.
	dependenciesToInclude := self transitiveDependenciesOf: aPackageName withoutDependenciesOf: aListOfPackageNameToIgnore.
	warnings 
		select: [:warning | dependenciesToInclude includes: warning package ]
		thenDo: [:warning | report addWarning: warning ].
	dependencies associationsDo: [ :assoc |
		((dependenciesToInclude includes: assoc key) 
		and: [ (aListOfPackageNameToIgnore includes: assoc key) not ])
			ifTrue: [ report 
							add: assoc key 
							dependencies: assoc value ] ].
	^ report