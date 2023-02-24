"
I'm a simple object superclass for objects that need to access information to be better displayed. 
In particular I offer to access to a ui theme and icon. 

For now I do not hold state but in the future it may make sense that I get an instance variable for the default theme and the icon manager. 
"
Class {
	#name : #DisplayableObject,
	#superclass : #Object,
	#category : #'System-Model-Base'
}

{ #category : #accessing }
DisplayableObject >> theme [

	^ Smalltalk ui theme
]
