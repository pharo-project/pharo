"
I am the superclass of all refactoring change objects. 
All I have is a name for the refactoring, but I can perform one or more refactoring operations with the message #execute. 
I am a composite object. To know about my components, ask me with #changes and #changesSize. 
"
Class {
	#name : 'ReChange',
	#superclass : 'Object',
	#instVars : [
		'name',
		'changeFactory',
		'onSystemEnvironment'
	],
	#category : 'Refactoring-Changes',
	#package : 'Refactoring-Changes'
}

{ #category : 'accessing' }
ReChange >> changeForClass: aClassName selector: aSelector [

	^ nil
]

{ #category : 'accessing' }
ReChange >> changeForMetaclass: aClassName selector: aSelector [

	^ nil
]

{ #category : 'accessing' }
ReChange >> changeStamp [

	^ Date today mmddyyyy , ' '
	  , ((String streamContents: [ :s | Time now print24: true on: s ])
			   copyFrom: 1
			   to: 5)
]

{ #category : 'printing' }
ReChange >> changeString [
	^self class name
]

{ #category : 'accessing' }
ReChange >> changes [

	^ Array with: self
]

{ #category : 'accessing' }
ReChange >> changesSize [

	^ 1
]

{ #category : 'printing' }
ReChange >> changesToDisplayIn: aChangeBrowser [

	^ { self }
]

{ #category : 'displaying' }
ReChange >> displayString [

	^ name ifNil: [ self changeString ] ifNotNil: [ name ]
]

{ #category : 'performing-changes' }
ReChange >> execute [
	^self executeNotifying: []
]

{ #category : 'private' }
ReChange >> executeNotifying: aBlock [
	self subclassResponsibility
]

{ #category : 'initialization' }
ReChange >> initialize [
	super initialize.
	onSystemEnvironment := Smalltalk globals.
	changeFactory := ReChangeManager changeFactory
]

{ #category : 'testing' }
ReChange >> isInScope: anEnvironment [

	^ self subclassResponsibility
]

{ #category : 'accessing' }
ReChange >> name [

	^ name ifNil: [ self changeString ] ifNotNil: [ name ]
]

{ #category : 'initialization' }
ReChange >> name: aString [
	
	name := aString
]

{ #category : 'accessing' }
ReChange >> nameToDisplay [

	^ self name
]

{ #category : 'accessing' }
ReChange >> oldVersionTextToDisplay [

	^ ''
]

{ #category : 'accessing' }
ReChange >> onSystemEnvironment [

	^ onSystemEnvironment
]

{ #category : 'accessing' }
ReChange >> onSystemEnvironment: anObject [

	onSystemEnvironment := anObject
]

{ #category : 'accessing' }
ReChange >> renameChangesForClass: oldClassName to: newClassName [
	"We're in the middle of performing a rename operation. If we stored the class name, we need to change the class name to the new name to perform the compiles."

	self subclassResponsibility
]

{ #category : 'accessing' }
ReChange >> textToDisplay [

	^ self name
]
