Class {
	#name : #MetacelloSkipDirtyPackageLoad,
	#superclass : #Notification,
	#instVars : [
		'packageSpec'
	],
	#category : #'Metacello-Core-Exceptions-Notifications'
}

{ #category : #signalling }
MetacelloSkipDirtyPackageLoad class >> signal: aMetacelloPackageSpec [

	^(self new packageSpec: aMetacelloPackageSpec) signal
]

{ #category : #accessing }
MetacelloSkipDirtyPackageLoad >> defaultAction [
	"Default action is to skip the load of a dirty package"

	^true
]

{ #category : #accessing }
MetacelloSkipDirtyPackageLoad >> packageSpec [

	^packageSpec
]

{ #category : #accessing }
MetacelloSkipDirtyPackageLoad >> packageSpec: aMetacelloPackageSpec [

	packageSpec := aMetacelloPackageSpec
]
