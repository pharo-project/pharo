"
I represent a result in the ̀Finder̀ tool.

I am a composite object and subclasses of me reperesent different kinds of results.

I have a content, which is the actual result, and define a specified set of actions
for it.
"
Class {
	#name : #FinderResult,
	#superclass : #Object,
	#instVars : [
		'content',
		'parent',
		'children'
	],
	#category : #'Tool-Finder-Result'
}

{ #category : #adding }
FinderResult >> addChild: aFinderResult [

	aFinderResult parent: self.
	children add: aFinderResult
]

{ #category : #action }
FinderResult >> browseAction [
	"Default does nothing. Subclasses may implement their own action for browsing."
]

{ #category : #accessing }
FinderResult >> children [

	^ children
]

{ #category : #accessing }
FinderResult >> children: anObject [

	children := anObject
]

{ #category : #accessing }
FinderResult >> content [

	^ content
]

{ #category : #accessing }
FinderResult >> content: anObject [

	content := anObject
]

{ #category : #displaying }
FinderResult >> displayString [

	^ content asString
]

{ #category : #testing }
FinderResult >> hasBrowseAction [

	^ false
]

{ #category : #testing }
FinderResult >> hasHierarchyAction [

	^ false
]

{ #category : #testing }
FinderResult >> hasImplementersAction [

	^ false
]

{ #category : #testing }
FinderResult >> hasInheritanceAction [

	^ false
]

{ #category : #testing }
FinderResult >> hasSendersAction [

	^ false
]

{ #category : #testing }
FinderResult >> hasVersionsAction [

	^ false
]

{ #category : #action }
FinderResult >> hierarchyAction [
	"Default does nothing. Subclasses may implement their own action for hieracchy."
]

{ #category : #action }
FinderResult >> implementersAction [
	"Default does nothing. Subclasses may implement their own action for implementers."
]

{ #category : #action }
FinderResult >> inheritanceAction [
	"Default does nothing. Subclasses may implement their own action for inheritance."
]

{ #category : #initialization }
FinderResult >> initialize [

	children := OrderedCollection new
]

{ #category : #testing }
FinderResult >> isClassResult [

	^ false
]

{ #category : #testing }
FinderResult >> isSelectorResult [

	^ false
]

{ #category : #accessing }
FinderResult >> parent [

	^ parent
]

{ #category : #accessing }
FinderResult >> parent: anObject [

	parent := anObject
]

{ #category : #action }
FinderResult >> sendersAction [
	"Default does nothing. Subclasses may implement their own action for senders."
]

{ #category : #action }
FinderResult >> versionsAction [
	"Default does nothing. Subclasses may implement their own action for versions."
]
