"
This announcement is emitted when we RE-compile a method in a class or a trait, with: ClassDescription >> compile: or TraitDescription >> compile:. If the method is not yet registered in the class or the trait, the announcement will not be emitted.

The action of renaming a method will be handled by SystemMethodRemovedAnnouncement and SystemMethodAddedAnnouncement, since this refactoring is concretely composed by removing the old method and add a new with the new name  
"
Class {
	#name : #MethodModified,
	#superclass : #MethodAnnouncement,
	#instVars : [
		'oldMethod',
		'oldProtocol'
	],
	#category : #'System-Announcements-System-Methods'
}

{ #category : #'instance creation' }
MethodModified class >> methodChangedFrom: oldMethod to: newMethod oldProtocol: oldProtocol [ 
	^ self new
		method: newMethod;
		oldMethod: oldMethod;
		oldProtocol: oldProtocol;
		yourself
]

{ #category : #testing }
MethodModified >> affectsMethod: aMethod [
	^(super affectsMethod: aMethod) or: [oldMethod == aMethod]
]

{ #category : #testing }
MethodModified >> affectsMethodsDefinedInPackage: aPackage [

	^(super affectsMethodsDefinedInPackage: aPackage) or: [ 
		oldMethod package == aPackage ]
]

{ #category : #testing }
MethodModified >> affectsMethodsTaggedWith: tagName [
	^(super affectsMethodsTaggedWith: tagName) or: [ oldProtocol = tagName ]
]

{ #category : #accessing }
MethodModified >> newMethod [

	^ method
]

{ #category : #accessing }
MethodModified >> newProtocol [
	
	^ self protocol
]

{ #category : #accessing }
MethodModified >> oldMethod [
	^ oldMethod
]

{ #category : #accessing }
MethodModified >> oldMethod: aCompiledMethod [
	oldMethod := aCompiledMethod.
]

{ #category : #accessing }
MethodModified >> oldProtocol [
	
	^ oldProtocol
]

{ #category : #accessing }
MethodModified >> oldProtocol: anObject [
	
	oldProtocol := anObject
]
