"
A package from the bootstrap layer (minimal image) is depending on an external package.
"
Class {
	#name : 'PharoBootstrapRule',
	#superclass : 'ReAbstractRule',
	#classVars : [
		'DependencyChecker'
	],
	#category : 'Specific-Rules',
	#package : 'Specific-Rules'
}

{ #category : 'testing' }
PharoBootstrapRule class >> checksPackage [

	^ true
]

{ #category : 'running' }
PharoBootstrapRule >> basicCheck: aPackage [

	(self bootstrapPackages includes: aPackage name) ifFalse: [ ^ false "Do not check" ].
	^ self newExternalDependencyExistFor: aPackage
]

{ #category : 'accessing' }
PharoBootstrapRule >> bootstrapPackages [
	"This should probably be improved"

	^ BaselineOfPharoBootstrap kernelPackageNames , #( 'AST-Core' )
]

{ #category : 'helpers' }
PharoBootstrapRule >> critiqueFor: aPackage [

	^ ArchitecturalCritique
		withAnchor: (self anchorFor: aPackage)
		by: self
]

{ #category : 'private' }
PharoBootstrapRule >> dependencyChecker [
	^ (DependencyChecker ifNil: [ DADependencyChecker ]) new
]

{ #category : 'accessing' }
PharoBootstrapRule >> group [

	^ 'Architectural'
]

{ #category : 'accessing' }
PharoBootstrapRule >> name [
	^ 'Illegal dependency of Bootstrap Layer'
]

{ #category : 'instance creation' }
PharoBootstrapRule >> newExternalDependencyExistFor: aPackage [
	"Check if there are new dependencies to a package outside this layer"

	^ ((self dependencyChecker dependenciesOf: aPackage name) difference: self bootstrapPackages) isNotEmpty
]
