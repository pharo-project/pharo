"
A Hudson report
"
Class {
	#name : #HDReport,
	#superclass : #Object,
	#category : #'JenkinsTools-Core'
}

{ #category : #running }
HDReport class >> runCategories: aCollectionOfStrings [
	^ aCollectionOfStrings do: [ :each | self runCategory: each ]
]

{ #category : #running }
HDReport class >> runCategory: aString [
	^ self runClasses: (self packageOrganizer classesInCategory: aString) named: aString
]

{ #category : #running }
HDReport class >> runClasses: aCollectionOfClasses named: aString [
	self subclassResponsibility
]

{ #category : #running }
HDReport class >> runPackage: aString [
	self subclassResponsibility
]

{ #category : #running }
HDReport class >> runPackages: aCollectionOfStrings [

	^ aCollectionOfStrings collect: [ :packageName |
		  Stdio stdout << 'Running tests of ' << packageName << OSPlatform current lineEnding.
		  self runPackage: packageName ]
]

{ #category : #private }
HDReport >> convert: aString [
	^ (aString asString 
		copyReplaceAll: (String with: Character cr with: Character lf) with: (String with: Character lf))
		copyReplaceAll: (String with: Character cr) with: (String with: Character lf)
]

{ #category : #private }
HDReport >> encode: aString [
	^ ((aString asString 
		copyReplaceAll: '&' with: '&amp;') 
		copyReplaceAll: '"' with: '&quot;') 
		copyReplaceAll: '<' with: '&lt;'
]
