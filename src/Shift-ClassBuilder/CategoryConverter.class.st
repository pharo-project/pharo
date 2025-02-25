"
I am a class used to manage some compatibility with the concept of old class categories. 

The categories were a mix of package names and package tag names but this had the drawback that it was not possible to know for sure what was what.

Since multiple places relied on categories, I am here to emulate the old behavior.
"
Class {
	#name : 'CategoryConverter',
	#superclass : 'Object',
	#instVars : [
		'packageName',
		'tagName',
		'environment'
	],
	#category : 'Shift-ClassBuilder-Builder',
	#package : 'Shift-ClassBuilder',
	#tag : 'Builder'
}

{ #category : 'class initialization' }
CategoryConverter class >> category: aString [

	^ self new processCategory: aString
]

{ #category : 'class initialization' }
CategoryConverter class >> category: aString environment: anEnvironment [

	^ self new
		  environment: anEnvironment;
		  processCategory: aString
]

{ #category : 'accessing' }
CategoryConverter >> environment [

	^ environment ifNil: [ self class environment ]
]

{ #category : 'accessing' }
CategoryConverter >> environment: anObject [

	environment := anObject
]

{ #category : 'accessing' }
CategoryConverter >> packageName [

	^ packageName
]

{ #category : 'processing' }
CategoryConverter >> processCategory: aString [
	"We are looking for the packages that matches the best the category and we consider the tag is what is left."

	(self environment organization packageMatchingExtensionName: aString)
		ifNil: [ packageName := aString ]
		ifNotNil: [ :package |
			packageName := package name.
			aString = packageName ifFalse: [ tagName := aString withoutPrefix: packageName , '-' ] ]
]

{ #category : 'accessing' }
CategoryConverter >> tagName [

	^ tagName
]
