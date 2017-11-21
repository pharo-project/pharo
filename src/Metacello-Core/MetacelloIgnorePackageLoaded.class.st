Class {
	#name : #MetacelloIgnorePackageLoaded,
	#superclass : #Notification,
	#instVars : [
		'packageSpec'
	],
	#category : #Metacello-Core-Exceptions-Notifications
}

{ #category : #signalling }
MetacelloIgnorePackageLoaded class >> signal: aMetacelloPackageSpec [

	^(self new packageSpec: aMetacelloPackageSpec) signal
]

{ #category : #accessing }
MetacelloIgnorePackageLoaded >> defaultAction [
  "Default action is to NOT ignore whether or not the package is loaded. 
   Basically allow for package level ignoreImage. "

  ^ false
]

{ #category : #accessing }
MetacelloIgnorePackageLoaded >> packageSpec [

   ^packageSpec
]

{ #category : #accessing }
MetacelloIgnorePackageLoaded >> packageSpec: anObject [

   packageSpec := anObject
]
