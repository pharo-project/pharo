"
I am the superclass of all refactoring change objects. 
All I have is a name for the refactoring, but I can perform one or more refactoring operations with the message #execute. 
I am a composite object. To know about my components, ask me with #changes and #changesSize. 
"
Class {
	#name : #RBRefactoryChange,
	#superclass : #Object,
	#instVars : [
		'name',
		'factory',
		'changeFactory',
		'onSystemDictionary'
	],
	#category : #'Refactoring-Changes'
}

{ #category : #accessing }
RBRefactoryChange >> changeForClass: aClassName selector: aSelector [ 
	^ nil
]

{ #category : #accessing }
RBRefactoryChange >> changeForMetaclass: aClassName selector: aSelector [ 
	^ nil
]

{ #category : #accessing }
RBRefactoryChange >> changeStamp [
	^ Author changeStamp
]

{ #category : #printing }
RBRefactoryChange >> changeString [
	^self class name
]

{ #category : #accessing }
RBRefactoryChange >> changes [
	^Array with: self
]

{ #category : #accessing }
RBRefactoryChange >> changesSize [
	^1
]

{ #category : #printing }
RBRefactoryChange >> displayString [

	^ name ifNil: [ self changeString ] ifNotNil: [ name ]
]

{ #category : #'performing-changes' }
RBRefactoryChange >> execute [
	^self executeNotifying: []
]

{ #category : #private }
RBRefactoryChange >> executeNotifying: aBlock [ 
	self subclassResponsibility
]

{ #category : #initialization }
RBRefactoryChange >> initialize [ 
	super initialize.
	onSystemDictionary := Smalltalk globals.
	changeFactory := RBRefactoryChangeManager changeFactory
]

{ #category : #accessing }
RBRefactoryChange >> name [

	^ name ifNil: [ self changeString ] ifNotNil: [ name ]
]

{ #category : #initialization }
RBRefactoryChange >> name: aString [
	name := aString
]

{ #category : #accessing }
RBRefactoryChange >> nameToDisplay [

	^ self name
]

{ #category : #accessing }
RBRefactoryChange >> oldVersionTextToDisplay [

	^ ''
]

{ #category : #accessing }
RBRefactoryChange >> onSystemDictionary [
	^ onSystemDictionary
]

{ #category : #accessing }
RBRefactoryChange >> onSystemDictionary: anObject [
	onSystemDictionary := anObject
]

{ #category : #accessing }
RBRefactoryChange >> renameChangesForClass: oldClassName to: newClassName [
	"We're in the middle of performing a rename operation. If we stored the class name, we need to change the class name to the new name to perform the compiles."

	self subclassResponsibility
]

{ #category : #accessing }
RBRefactoryChange >> textToDisplay [

	^ self name
]
