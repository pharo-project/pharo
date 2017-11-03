"
This announcement will be emitted when a class or a trait definition changes and completally installed.

It is different than ClassModifiedClassDefinition because it signalled only after new version of class/trait object is installed to system by become operation.  And that's why only modifiedClass variable is available (after #becomeForward: operation old version of class/trait disappear)
"
Class {
	#name : #ClassModificationApplied,
	#superclass : #ClassAnnouncement,
	#instVars : [
		'modifiedClass'
	],
	#category : #System-Announcements-System-Classes
}

{ #category : #'instance creation' }
ClassModificationApplied class >> toClass: aClassOrTrait [

	^self new 
		modifiedClass: aClassOrTrait
]

{ #category : #accessing }
ClassModificationApplied >> classAffected [
	^modifiedClass 
]

{ #category : #accessing }
ClassModificationApplied >> modifiedClass: anObject [
	modifiedClass := anObject
]
