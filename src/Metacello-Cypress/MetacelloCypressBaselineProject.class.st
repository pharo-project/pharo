"
The **MetacelloCypressBaselineProject**  is a wrapper for the **BaselineOf** version specification for file-based repositories specific to metadataless export format. (Filetree/Tonel)

It should be used by metadataless repositories (FileTree/Tonel). To use it the **BaselineOf** needs to redefine this method:

```Smalltalk
	projectClass
	    ^ MetacelloMCBaselineProject
```

Metacello has an internal rule to not load Monticello packages of the same version, since they are already loaded. However, when using metadataless repositories the filetree/tonel Monticello package readers typically generate a package name using the author/version `-cypress.1`, which make Metacello think that the versions are the same and the package is not loaded. By including the above method in your baselineof, Metacello will know to ignore the Monticello author/version of the package and always load it.
"
Class {
	#name : #MetacelloCypressBaselineProject,
	#superclass : #MetacelloMCBaselineProject,
	#classVars : [
		'UseCypressPackagesForAllBaselines'
	],
	#category : #'Metacello-Cypress'
}

{ #category : #accessing }
MetacelloCypressBaselineProject class >> singletonVersionName [
    ^ 'baseline'
]

{ #category : #accessing }
MetacelloCypressBaselineProject class >> useCypressPackagesForAllBaselines [
  UseCypressPackagesForAllBaselines ifNil: [ ^ false ].
  ^ UseCypressPackagesForAllBaselines
]

{ #category : #accessing }
MetacelloCypressBaselineProject class >> useCypressPackagesForAllBaselines: aBool [
	"self useCypressPackagesForAllBaselines: true"

	UseCypressPackagesForAllBaselines := aBool
]

{ #category : #accessing }
MetacelloCypressBaselineProject class >> versionConstructorClass [
    ^ MetacelloBaselineConstructor
]

{ #category : #'spec classes' }
MetacelloCypressBaselineProject >> baselineOfProjectSpecClass [
    ^ MetacelloCypressBaselineProjectSpec
]

{ #category : #'spec classes' }
MetacelloCypressBaselineProject >> packageSpecClass [
  ^ MetacelloCypressPackageSpec
]
