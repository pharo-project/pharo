"
I am a contact book. I hold a list of contacts.
"
Class {
	#name : #SpContactBook,
	#superclass : #Object,
	#instVars : [
		'contents'
	],
	#classInstVars : [
		'family',
		'coworkers'
	],
	#category : #'Spec2-Commander2-ContactBook-Model'
}

{ #category : #accessing }
SpContactBook class >> coworkers [
	^coworkers ifNil: [ 
		coworkers := self new 
			add: 'Stef' phone: '112 378';
			add: 'Pavel' phone: '898 678';
			add: 'Marcus' phone: '444 888';
			yourself]
]

{ #category : #accessing }
SpContactBook class >> family [
	^family ifNil: [ 
		family := self new 
			add: 'John' phone: '342 345';
			add: 'Bill' phone: '123 678';
			add: 'Marry' phone: '789 567';
			yourself]
]

{ #category : #initialization }
SpContactBook class >> reset [
	<script>
	coworkers := nil.
	family := nil
]

{ #category : #accessing }
SpContactBook >> add: contactName phone: phone [
	| contact |
	contact := SpContact named: contactName phone: phone.
	self addContact: contact.
	^contact
]

{ #category : #accessing }
SpContactBook >> addContact: aContact [
	contents add: aContact
]

{ #category : #accessing }
SpContactBook >> addContact: newContact after: contactAfter [ 
	contents add: newContact after: contactAfter
]

{ #category : #accessing }
SpContactBook >> contents [
	^ contents
]

{ #category : #accessing }
SpContactBook >> contents: anObject [
	contents := anObject
]

{ #category : #testing }
SpContactBook >> includesContact: aContact [
	^ contents includes: aContact
	
]

{ #category : #initialization }
SpContactBook >> initialize [
	super initialize.
	contents := OrderedCollection new.
]

{ #category : #accessing }
SpContactBook >> removeContact: aContact [
	contents remove: aContact
]
