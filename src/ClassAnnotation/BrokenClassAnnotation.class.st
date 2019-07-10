"
I represent broken class annotation when declaration method raises error during registry building.

I include description of raisen error in my variable errorString.

To debug broken annotation you can inspect all of them using:
	BrokenClassMetaAnnotation registeredInstances 
Then ask #debug to choosen annotation:
	brokenAnnotation debug.
It perform declaration method which will open debugger.	

My instances are created using mesage #withError:
	BrokenClassAnnotation withError: anError
I only collect description of given error to not keep garbage of failed process.	

Internal Representation and Key Implementation Points.

    Instance Variables
	errorString:		<String>

"
Class {
	#name : #BrokenClassAnnotation,
	#superclass : #ClassAnnotation,
	#instVars : [
		'errorString'
	],
	#category : #ClassAnnotation
}

{ #category : #'instance creation' }
BrokenClassAnnotation class >> forBadMethodReturn [
	^self new 
		errorString: 'Annotating method do not return an annotation instance'
]

{ #category : #'instance creation' }
BrokenClassAnnotation class >> withError: anError [
	^self new 
		errorString: anError description
]

{ #category : #operations }
BrokenClassAnnotation >> debug [
	"here we just reevaluate messages which could fail"
	| ann |
	ann := self freshInstance.
	ann isForbidden.
	^ann
]

{ #category : #accessing }
BrokenClassAnnotation >> errorString [
	^ errorString
]

{ #category : #accessing }
BrokenClassAnnotation >> errorString: anObject [
	errorString := anObject
]

{ #category : #printing }
BrokenClassAnnotation >> printOn: aStream [
	super printOn: aStream.
	
	self printDeclarationOn: aStream
]
