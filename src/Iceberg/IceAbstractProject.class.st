Class {
	#name : #IceAbstractProject,
	#superclass : #Object,
	#instVars : [
		'commitish'
	],
	#category : #'Iceberg-Project'
}

{ #category : #'instance creation' }
IceAbstractProject class >> onCommitish: anIceCommitish [
	
	^ self new
		commitish: anIceCommitish;
		yourself
]

{ #category : #'instance creation' }
IceAbstractProject class >> onRepository: aRepository [
	
	^ self onCommitish: aRepository workingCopy
]

{ #category : #comparing }
IceAbstractProject >> = anotherProject [

	self species = anotherProject species ifFalse: [ ^ false ].
	self repository = anotherProject repository ifFalse: [ ^ false ]. 
	self properties = anotherProject properties ifFalse: [ ^ false ].
	^ true.
]

{ #category : #writing }
IceAbstractProject >> addToChangeBuilder: anIceChangeImporter [ 
	
	"Nothing by default"
]

{ #category : #accessing }
IceAbstractProject >> commitish: anIceCommitish [
	
	commitish := anIceCommitish
]

{ #category : #testing }
IceAbstractProject >> isDirty [
	"Compare against the project extracted from the HEAD commit.
	If the commit has no project, it will be an unborn project."
	^ self repository workingCopy referenceCommit project ~= self readOnlyVersion
]

{ #category : #testing }
IceAbstractProject >> isInvalid [
	
	^ false
]

{ #category : #testing }
IceAbstractProject >> isUnborn [
	
	^ false
]

{ #category : #'API-packages' }
IceAbstractProject >> packages [
	
	self repository handleLibgitError: [ | writerClass sourceFileReference |
		writerClass := self writerClass.
		sourceFileReference := commitish fileSystem resolve: self sourceDirectoryPath.
		sourceFileReference exists ifFalse: [ ^ #() ].
		
		^ sourceFileReference children
			select: [ :each | writerClass isValidPackage: each ]
			thenCollect: [ :each | | packageName |
				packageName := writerClass packageNameFromDirectory: each.
				IcePackage named: packageName repository: self repository ] ]
]

{ #category : #'API-packages' }
IceAbstractProject >> packagesFrom: aCommit [
	
	self repository handleLibgitError: [ | writerClass sourceFileReference |
		writerClass := self writerClass.
		sourceFileReference :=  aCommit fileSystem resolve: self sourceDirectory.
		sourceFileReference exists ifFalse: [ ^ #() ].
		
		^ sourceFileReference children
			select: [ :each | writerClass isValidPackage: each ]
			thenCollect: [ :each | | packageName |
				packageName := writerClass packageNameFromDirectory: each basename.
				IcePackage named: packageName repository: self repository ] ]
]

{ #category : #accessing }
IceAbstractProject >> properties [

	^ Dictionary new
]

{ #category : #testing }
IceAbstractProject >> readOnlyVersion [

	^ self
]

{ #category : #accessing }
IceAbstractProject >> repository [
	
	^ commitish repository
]

{ #category : #accessing }
IceAbstractProject >> sourceDirectory [
	
	^ self subclassResponsibility
]

{ #category : #accessing }
IceAbstractProject >> sourceDirectoryPath [

	"API compatible both with Pharo6 and Pharo7.
	This could be replaced by the Pharo7 API
	
	RelativePath with: self subdirectory
	"
	^ (RelativePath withAll: (RelativePath canonicalizeElements: ($/ split: self sourceDirectory)))
]

{ #category : #accessing }
IceAbstractProject >> sourceDirectoryReference [
	
	^ commitish fileSystem resolve: self sourceDirectoryPath
]

{ #category : #accessing }
IceAbstractProject >> writerClass [
	
	^ self repository properties writerClass
]
