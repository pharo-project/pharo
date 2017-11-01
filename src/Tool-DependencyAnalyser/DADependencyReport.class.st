"
I'm reporting dependencies of packages previously analysed byt the dependnecy checker.
"
Class {
	#name : #DADependencyReport,
	#superclass : #Object,
	#instVars : [
		'dependencies',
		'warnings',
		'imageVersion',
		'analysisRunString'
	],
	#category : 'Tool-DependencyAnalyser-Report'
}

{ #category : #example }
DADependencyReport class >> example [
	^ self new 
		add: #A dependencies: #();
		add: #B dependencies: #(#A);
		add: #C dependencies: #(#A #B);
		add: #D dependencies: #(#C #A #E);
		add: #E dependencies: #(#D);
		yourself
]

{ #category : #adding }
DADependencyReport >> add: aPackageName dependencies: aListOfPackages [
	dependencies 
		at: aPackageName 
		put: aListOfPackages
]

{ #category : #adding }
DADependencyReport >> addWarning: aWarning [ 
	warnings add: aWarning
]

{ #category : #accessing }
DADependencyReport >> allPackages [
	^ Set new
		addAll: dependencies keys;
		addAll: dependencies values flattened;
		yourself
		
]

{ #category : #accessing }
DADependencyReport >> analysisRunString [
	^ analysisRunString
]

{ #category : #accessing }
DADependencyReport >> analysisRunString: aString [
	analysisRunString := aString
]

{ #category : #iterating }
DADependencyReport >> dependenciesAndKnownDependantsDo: aBlockClosure [ 
	dependencies keysAndValuesDo: [ :pkgName :deps |
		aBlockClosure 
			value: pkgName
			value: deps
			value: (self knownDependantsOf: pkgName) ]
]

{ #category : #iterating }
DADependencyReport >> dependenciesDo: aBlockClosure [ 
	dependencies keysAndValuesDo: aBlockClosure
]

{ #category : #accessing }
DADependencyReport >> dependenciesOf: aPackageName [
	^ dependencies at: aPackageName
]

{ #category : #accessing }
DADependencyReport >> dependenciesOf: aPackageName ifAbsentPut: aBlock [
	^ dependencies 
		at: aPackageName
		put: aBlock value
]

{ #category : #accessing }
DADependencyReport >> imageVersion [
	^ imageVersion
]

{ #category : #accessing }
DADependencyReport >> imageVersion: aSystemVersion [ 
	imageVersion := aSystemVersion
]

{ #category : #initialization }
DADependencyReport >> initialize [
	super initialize.
	dependencies := Dictionary new.
	warnings := Set new
]

{ #category : #accessing }
DADependencyReport >> knownDependantsOf: aPackageName [
	^ dependencies associations 
		select: [ :association | association value includes: aPackageName ]
		thenCollect: #key
]

{ #category : #converting }
DADependencyReport >> newReportScopedOn: aPackageName withoutDependencies: aListOfPackageNameToIgnore [
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
]

{ #category : #accessing }
DADependencyReport >> transitiveDependenciesOf: aPackageName [
	^ self transitiveDependenciesOf: aPackageName visited: Set new
]

{ #category : #private }
DADependencyReport >> transitiveDependenciesOf: aPackageName visited: alreadyVisitedPackages [
	| deps |
	deps := Set new.
	alreadyVisitedPackages add: aPackageName.
	deps 
		addAll: (dependencies at: aPackageName).
	deps
		reject: [ :packageName | alreadyVisitedPackages includes: packageName ]
		thenDo: [ :packageName | deps addAll: (self transitiveDependenciesOf: packageName visited: alreadyVisitedPackages) ].
	^ deps
]

{ #category : #accessing }
DADependencyReport >> transitiveDependenciesOf: aPackageName withoutDependenciesOf: aListOfPackageNames [
	"Do not add noise to the analysis by adding transitive dependencies of dependencies we want to remove."
	^ self 
		transitiveDependenciesOf: aPackageName
		visited: (aListOfPackageNames collect: #asSymbol as: Set)
]

{ #category : #accessing }
DADependencyReport >> warnings [
	^ warnings
]
