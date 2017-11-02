"
A TrivialCritique with and extended actions to open a dependency browser for ArchitecturalRule
"
Class {
	#name : #ArchitecturalCritique,
	#superclass : #ReTrivialCritique,
	#category : 'Kernel-Rules'
}

{ #category : #actions }
ArchitecturalCritique >> actions [
	^ super actions copyWith: 
		(RePropertyAction new
			icon: #smallWindow asIcon;
			description: 'Open dependency analyzer';
			action: [ :crit :package |
				self openDependencyAnalyzerOn: package ];
			yourself)
				
]

{ #category : #actions }
ArchitecturalCritique >> openDependencyAnalyzerOn: package [

	^ (DAPackageAnalyzerPackageDependenciesWindow
			onPackagesNamed: {package packageName}) open
]
