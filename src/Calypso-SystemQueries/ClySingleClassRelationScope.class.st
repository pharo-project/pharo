"
My subclasses represents simple class hierarchy scope which is built using single relationship between classes.
For example ClySubclassScope shows only subclasses of scope basis where relationship superclass-subclass is only used. 

I delegate #classesRelatedTo:do: to the class side. So it can be used by classes themselves and not only instances.
Subclasses should implement this method on class side.
"
Class {
	#name : #ClySingleClassRelationScope,
	#superclass : #ClyClassHierarchyScope,
	#category : #'Calypso-SystemQueries-Scopes'
}

{ #category : #accessing }
ClySingleClassRelationScope class >> classesRelatedTo: aClass do: aBlock [
	self subclassResponsibility 
]

{ #category : #queries }
ClySingleClassRelationScope >> classesRelatedTo: aClass do: aBlock [
	self class classesRelatedTo: aClass do: aBlock
]
