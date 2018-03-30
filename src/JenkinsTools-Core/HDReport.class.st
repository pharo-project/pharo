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
	^ self runClasses: (Smalltalk organization classesInCategory: aString) named: aString
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
	^ aCollectionOfStrings collect: [ :each | self runPackage: each ]
]
