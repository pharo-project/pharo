"
I keep a symbol entry, along with the last time it was used in the system. 
I am used to sort the possible answers.
"
Class {
	#name : #NOCDatedEntry,
	#superclass : #NECSelectorEntry,
	#instVars : [
		'date',
		'next',
		'previous'
	],
	#category : 'NECompletion-OCompletion'
}

{ #category : #operations }
NOCDatedEntry >> <= anEntry [
	^ date = anEntry date 
		ifTrue: [contents <= anEntry contents]
		ifFalse: [date > anEntry date]
]

{ #category : #accessing }
NOCDatedEntry >> date [
	^ date
]

{ #category : #accessing }
NOCDatedEntry >> date: d [
	date := d
]

{ #category : #accessing }
NOCDatedEntry >> link: anODatedEntry [
	"Link the given entry after me."

	anODatedEntry
		next: next;
		previous: self.
	next previous: anODatedEntry.
	next := anODatedEntry
]

{ #category : #accessing }
NOCDatedEntry >> matches: pref [

	^contents beginsWithEmpty: pref caseSensitive: false
]

{ #category : #accessing }
NOCDatedEntry >> next [

	^next
]

{ #category : #accessing }
NOCDatedEntry >> next: anODatedEntry [

	next := anODatedEntry
]

{ #category : #accessing }
NOCDatedEntry >> now [
	date := DateAndTime now
]

{ #category : #accessing }
NOCDatedEntry >> previous [

	^previous
]

{ #category : #accessing }
NOCDatedEntry >> previous: anODatedEntry [

	previous := anODatedEntry
]

{ #category : #accessing }
NOCDatedEntry >> unlink [

	next previous: previous.
	previous next: next
]
