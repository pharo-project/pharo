"
I'm a general strategy to perform checkouts. 
In Pharo, checkouts are a lot more complicated than a simple checkout in file system, since we have packages installed that corresponds with the commit of reference.
Doing a checkout then means also introduce new versions into the image, and there are different possible strategies to perform this task in a safe way. 
My children will implement method #checkoutPackagesOn: to perform this task.
"
Class {
	#name : #IceCheckoutStrategy,
	#superclass : #Object,
	#instVars : [
		'committish',
		'diff'
	],
	#category : #'Iceberg-Core'
}

{ #category : #accessing }
IceCheckoutStrategy class >> allStrategies [
	^ (self allSubclasses 
		sorted: [ :a :b | a order < b order ])
		collect: #uniqueInstance
]

{ #category : #accessing }
IceCheckoutStrategy class >> order [
	^ self subclassResponsibility
]

{ #category : #checkout }
IceCheckoutStrategy >> checkoutOn: anIceWorkingCopy [ 
	
	self subclassResponsibility
]

{ #category : #accessing }
IceCheckoutStrategy >> committish: aCommittish [
	
	committish := aCommittish
]

{ #category : #accessing }
IceCheckoutStrategy >> diff [
	
	^ diff ifNil: [ 
		diff := committish diffTo: self workingCopy ].
]

{ #category : #accessing }
IceCheckoutStrategy >> diff: anIceDiff [ 
	
	diff := anIceDiff
]

{ #category : #accessing }
IceCheckoutStrategy >> repository [

	^ committish repository
]

{ #category : #accessing }
IceCheckoutStrategy >> workingCopy [
	
	^ self repository workingCopy
]
