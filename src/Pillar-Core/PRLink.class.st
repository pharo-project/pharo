"
I am an abstract link built from an alias and a reference. The alias is the string representation that will be displayed to the user, whereas the reference is a string identifying the target. If there is no alias, the reference itself is displayed. Links can try to embed the referenced target into the containing document.

!Examples
=*Reference*
=+Alias>Embedded Reference*
"
Class {
	#name : #PRLink,
	#superclass : #PRReference,
	#category : #'Pillar-Core-Model'
}

{ #category : #'instance creation' }
PRLink class >> fromReference: aReference [
	| classes  |
	classes := PRLink withAllConcreteClasses select: [ :each | each isValidReference: aReference ].
	classes sort: [ :a :b | a priority < b priority ].
	^ classes first reference: aReference
]

{ #category : #testing }
PRLink class >> isAbstract [ 

	^ true
]

{ #category : #accessing }
PRLink class >> priority [
	^ 0
]

{ #category : #visiting }
PRLink >> accept: aVisitor [
	<ignoreForCoverage "This method is ignored for test coverage because it is overriden in all subclasses and these subclasses don't need to do a super-send.">
	^ aVisitor visitLink: self
]

{ #category : #testing }
PRLink >> isBroken [
	"Answer ==true== if the receiver is broken."

	^ false
]

{ #category : #testing }
PRLink >> isExternal [
	^ false
]

{ #category : #testing }
PRLink >> isInternal [
	^ false
]

{ #category : #testing }
PRLink >> isLink [
	^ true
]

{ #category : #accessing }
PRLink >> renderAsText [
	^ self reference
]

{ #category : #accessing }
PRLink >> title [
	^ self renderAsText ifEmpty: [ nil ] ifNotEmpty: #yourself
]
