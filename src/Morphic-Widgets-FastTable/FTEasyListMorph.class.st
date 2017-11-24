"
I'm an easy list used to simplify the creation of lists.
My parent is too powerful, I'm just a simplification of it (and no, I will not go to the psy to deal with that).

 Example: 
=========
FTEasyListMorph new
	extent: 300@550;
	elements: Morph methods;
	display: [ :method | method selector ];		
	openInWindow.
	
"
Class {
	#name : #FTEasyListMorph,
	#superclass : #FTTableMorph,
	#category : #'Morphic-Widgets-FastTable-Easy'
}

{ #category : #accessing }
FTEasyListMorph >> display: aBlock [
	self dataSource display: aBlock
]

{ #category : #accessing }
FTEasyListMorph >> elements: aSequenceableCollection [
	self dataSource elements: aSequenceableCollection
]

{ #category : #accessing }
FTEasyListMorph >> header: aString [
	self ensureAtLeastOneColumn.
	self columns first id: aString
]

{ #category : #accessing }
FTEasyListMorph >> icon: aBlock [
	self dataSource icon: aBlock
]

{ #category : #initialization }
FTEasyListMorph >> initialize [
	super initialize.
	self dataSource: FTEasyListDataSource new
]

{ #category : #accessing }
FTEasyListMorph >> menu: aBlock [
	self dataSource menu: aBlock
]
