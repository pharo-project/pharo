"
I'm a specialized error that is invoked when trying to load a method for a non existing class. 

By default this exception is not resumable but it can be set and used for example as follow:


	[ FileStream fileIn: aFile ]
		on: MissingClassError
		do: [ :exception | 
				exception defineClass. 
				exception asResumable.
				exception resume. ]
			
			
So we give the possibility to compile and resume compution.
The method defineClass defines a simple class inheriting from Object (or from the class specified using #superclassName:)
The idea is that if later the effective class is loaded its definition will override  this one. 
"
Class {
	#name : #MissingClassError,
	#superclass : #Error,
	#instVars : [
		'resumable',
		'className',
		'superclassName'
	],
	#category : #'CodeImport-Errors'
}

{ #category : #'instance creation' }
MissingClassError class >> className: aString [

	^ self new className: aString; yourself
]

{ #category : #actions }
MissingClassError >> asResumable [
	"When sent to the receiver, this one turns into a resumable exception allowing for example to define missing class on the fly."
	resumable := true.
]

{ #category : #accessing }
MissingClassError >> className: aString [

	className := aString
]

{ #category : #actions }
MissingClassError >> defineClass [

	Smalltalk compiler evaluate: 
		self superclassName,  ' subclass: #', className, 
	' instanceVariableNames: ''''
	classVariableNames: ''''
	poolDictionaries: ''''
	category: ''Stub-Classes'''
]

{ #category : #testing }
MissingClassError >> isResumable [
	"Determine whether an exception is resumable. By default not but can be resumable using accessors at exception raising time."

	^ resumable ifNil: [ super isResumable ]
]

{ #category : #accessing }
MissingClassError >> superclassName [

	^ superclassName ifNil: [ superclassName := 'Object' ]

]

{ #category : #accessing }
MissingClassError >> superclassName: aString [

	superclassName := aString

]
