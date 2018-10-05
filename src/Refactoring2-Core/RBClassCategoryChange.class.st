"
I'm a change corresponding to a change in the class protocols. 
"
Class {
	#name : #RBClassCategoryChange,
	#superclass : #RBRefactoryClassChange,
	#instVars : [
		'category'
	],
	#category : #'Refactoring2-Core-Changes'
}

{ #category : #'instance creation' }
RBClassCategoryChange class >> category: aString for: aClass [

	^ self new
		changeClass: aClass;
		category: aString;
		yourself
]

{ #category : #converting }
RBClassCategoryChange >> asUndoOperation [

	self copy 
		category: self changeClass category;
		yourself
]

{ #category : #accessing }
RBClassCategoryChange >> category: anObject [
	category := anObject
]

{ #category : #accessing }
RBClassCategoryChange >> changeClass: aBehavior [

	super changeClass: aBehavior.
	isMeta := false.
]

{ #category : #printing }
RBClassCategoryChange >> changeString [

	^ 'Category ', self displayCategoryName, ' in ', self displayClassName
]

{ #category : #printing }
RBClassCategoryChange >> displayCategoryName [

	^ category
]

{ #category : #private }
RBClassCategoryChange >> primitiveExecute [
	
	self changeClass category: category.
	SystemAnnouncer uniqueInstance 
		classReorganized: self changeClass
]

{ #category : #printing }
RBClassCategoryChange >> printOn: aStream [ 

	aStream
		nextPutAll: self displayClassName;
		nextPutAll: ' classCategory: ';
		print: self displayCategoryName;
		nextPutAll: ' stamp: ';
		print: self changeStamp;
		nextPutAll: '!'
]
