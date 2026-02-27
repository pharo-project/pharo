"
I am a warning that is raised in the case the user try to make a method an extension method contained in the package containing the class. This should not happen because it would cause some incoherences in the system.
"
Class {
	#name : 'ExtensionPointsOwningPackageNotification',
	#superclass : 'SystemNotification',
	#instVars : [
		'packageName',
		'concernedClass',
		'selector'
	],
	#category : 'Kernel-Exceptions',
	#package : 'Kernel',
	#tag : 'Exceptions'
}

{ #category : 'signalling' }
ExtensionPointsOwningPackageNotification class >> signalFor: aPackageName class: aClass selector: aSelector [

	^ self new
		  packageName: aPackageName;
		  concernedClass: aClass;
		  selector: aSelector;
		  signal
]

{ #category : 'accessing' }
ExtensionPointsOwningPackageNotification >> concernedClass [

	^ concernedClass
]

{ #category : 'accessing' }
ExtensionPointsOwningPackageNotification >> concernedClass: anObject [

	concernedClass := anObject
]

{ #category : 'accessing' }
ExtensionPointsOwningPackageNotification >> messageText [

	^ messageText ifNil: [
			  'An extension protocol points the package "' , packageName , '" while ' , self concernedClass , ' containing ' , (self selector ifNil: [ 'the method' ])
			  , ' is already in this package. The creation of this protocol will be aborted.' ]
]

{ #category : 'accessing' }
ExtensionPointsOwningPackageNotification >> packageName [

	^ packageName
]

{ #category : 'accessing' }
ExtensionPointsOwningPackageNotification >> packageName: anObject [

	packageName := anObject
]

{ #category : 'accessing' }
ExtensionPointsOwningPackageNotification >> selector [

	^ selector
]

{ #category : 'accessing' }
ExtensionPointsOwningPackageNotification >> selector: anObject [

	selector := anObject
]
