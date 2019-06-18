"
I represent class annotation which annotates forbidden classes.
I am only used during registry bulding. And my instances are never added to the registry.

When annotation is attatched to forbidden class I am used instead during registry building. I guard actual annotation from being added to the registry.

I define #isForbidden method to be always true. So it never fails.
I was introduced to check actual annotation inside ensure block. So if #isForbidden method fails (due to bad implementation) the full annotation will be safelly replaced by BrokenClassAnnotation with details about error.
But if it not fails then such annotation is replaced by me. And outer place where annotation is added to the registry can safelly check #isForbidden without error guard.  
"
Class {
	#name : #ForbiddenClassAnnotation,
	#superclass : #ClassAnnotation,
	#category : #ClassAnnotation
}

{ #category : #accessing }
ForbiddenClassAnnotation >> actualAnnotation [
	^self freshInstance
]

{ #category : #testing }
ForbiddenClassAnnotation >> isForbidden [
	^true
]

{ #category : #printing }
ForbiddenClassAnnotation >> printOn: aStream [
	super printOn: aStream.
	
	self printDeclarationOn: aStream
]
