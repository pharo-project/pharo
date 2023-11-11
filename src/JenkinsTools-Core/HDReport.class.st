"
A Hudson report
"
Class {
	#name : 'HDReport',
	#superclass : 'Object',
	#category : 'JenkinsTools-Core',
	#package : 'JenkinsTools-Core'
}

{ #category : 'running' }
HDReport class >> runClasses: aCollectionOfClasses named: aString [
	self subclassResponsibility
]

{ #category : 'running' }
HDReport class >> runPackage: aString [
	self subclassResponsibility
]

{ #category : 'running' }
HDReport class >> runPackages: aCollectionOfStrings [

	^ aCollectionOfStrings collect: [ :packageName | self runPackage: packageName ]
]

{ #category : 'private' }
HDReport >> convert: aString [
	^ (aString asString 
		copyReplaceAll: (String with: Character cr with: Character lf) with: (String with: Character lf))
		copyReplaceAll: (String with: Character cr) with: (String with: Character lf)
]

{ #category : 'private' }
HDReport >> encode: aString [
	^ ((aString asString 
		copyReplaceAll: '&' with: '&amp;') 
		copyReplaceAll: '"' with: '&quot;') 
		copyReplaceAll: '<' with: '&lt;'
]
