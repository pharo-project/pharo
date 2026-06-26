Extension { #name : 'ClyTextEditor' }

{ #category : '*Calypso-SystemTools-QueryBrowser' }
ClyTextEditor >> classCommentsContainingIt [
	"Open a browser class comments which contain the current selection somewhere in them."

	| query |
	self lineSelectAndEmptyCheck: [^ self].

	query := ClyClassCommentsQuery withString: self selection string.
	self browser spawnQueryBrowserOn: query
]

{ #category : '*Calypso-SystemTools-QueryBrowser' }
ClyTextEditor >> classNamesContainingIt [
	"Open a browser on classes whose names contain the selected string"

	| query |
	self lineSelectAndEmptyCheck: [^self].

	query := ClyAllClassesQuery new filteredBy: (ClyItemNameFilter substringPattern: self selection string).
	self browser spawnQueryBrowserOn: query
]

{ #category : '*Calypso-SystemTools-QueryBrowser' }
ClyTextEditor >> implementorsOf: selectedSelector [

	self browser classSelection 
		ifEmpty: [ ^ (Smalltalk tools 
			toolNamed: #messageList) 
			browseImplementorsOf: selectedSelector ].
	
	(Smalltalk tools toolNamed: #messageList) 
		browseImplementorsOf: selectedSelector 
		from: self browser classSelection lastSelectedItem actualObject.
]

{ #category : '*Calypso-SystemTools-QueryBrowser' }
ClyTextEditor >> implementorsOfIt [

	| selector |
	(selector := self selectedSelector) ifNil: [ ^ textArea flash ].
	selector isCharacter ifTrue: [ ^ textArea flash ].
	self implementorsOf: selector
]

{ #category : '*Calypso-SystemTools-QueryBrowser' }
ClyTextEditor >> methodSourceContainingIt [
	"Open a browser class comments which contain the current selection somewhere in them."

	| pattern |
	self lineSelectAndEmptyCheck: [ ^ self ].
	pattern := self selection string.
	(Smalltalk tools toolNamed: #messageList) browseMethodsWithSourceString: pattern
]

{ #category : '*Calypso-SystemTools-QueryBrowser' }
ClyTextEditor >> referencesTo: aVariableName [
	"Open a references browser on the given symbol"

	| class |
	" ugly dispatch, but current Browser protocol names aren't really cool "
	class := self modelCurrentSelectedClass.
 
	class isBehavior ifTrue: [
			class
				slotNamed: aVariableName
				ifFound: [ :slot | ^ (Smalltalk tools toolNamed: #messageList) browseReferencesToVariable: slot inClass: class ] ].

	self browser browseReferencesTo: aVariableName asSymbol inNameResolver: class
]

{ #category : '*Calypso-SystemTools-QueryBrowser' }
ClyTextEditor >> referencesToIt [

	| selector |
	"self lineSelectAndEmptyCheck: [^ self]."
	(selector := self selectedSelector) ifNil: [ ^ textArea flash ].

	self referencesTo: selector
]

{ #category : '*Calypso-SystemTools-QueryBrowser' }
ClyTextEditor >> sendersOf: selectedSelector [

	self browser classSelection ifEmpty: [
		^ self browser browseReferencesTo: selectedSelector inNameResolver: self browserTool selectedClassOrMetaClass ].

	self browser browseReferencesTo: selectedSelector inNameResolver: self browserTool selectedClassOrMetaClass 
]

{ #category : '*Calypso-SystemTools-QueryBrowser' }
ClyTextEditor >> sendersOfIt [

	| selector |
	(selector := self selectedSelector) ifNil: [ ^ textArea flash ].
	self sendersOf: selector
]
