"
I am a package spec specific to metadataless projects (filetree/tonel).
"
Class {
	#name : #MetacelloCypressPackageSpec,
	#superclass : #MetacelloPackageSpec,
	#category : #'Metacello-Cypress-Specs'
}

{ #category : #querying }
MetacelloCypressPackageSpec >> ancestors [
  ^ nil
]

{ #category : #testing }
MetacelloCypressPackageSpec >> compareCurrentVersion: anOperator targetVersionStatus: statusIgnored using: anMCLoader [
  ^ false
]

{ #category : #querying }
MetacelloCypressPackageSpec >> isPackageLoaded: aLoader [
  MCWorkingCopy allManagers
    detect: [ :wc | wc packageName = self file ]
    ifNone: [ ^ false ].
  ^ true
]

{ #category : #fetching }
MetacelloCypressPackageSpec >> searchCacheRepositoryForPackage: searchBlock [
  "evaluate the <searchBlock> if you want to search for the package in a local package cache"

  "for Cypress packages the answer is NO!"


]
