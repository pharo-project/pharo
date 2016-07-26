change set
findChangeSetIn: aCollectionOfChangeSets
	"Search for a changeSet by name.  Pop up a menu of all changeSets whose name contains the string entered by the user.  If only one matches, then the pop-up menu is bypassed"
	| index pattern candidates nameList |
	self okToChange ifFalse: [^ self].
	pattern := UIManager default request: 'ChangeSet name or fragment?'.
	pattern isEmptyOrNil ifTrue: [^ self].
	nameList := aCollectionOfChangeSets collect: #name.
	candidates := aCollectionOfChangeSets select:
			[:c | (nameList includes: c name) and: 
				[c name includesSubstring: pattern caseSensitive: false]].
	candidates size = 0 ifTrue: [ ^ self inform: 'No matching change sets.' ].
	candidates size = 1 ifTrue: [ ^ candidates first ].
	index := UIManager default chooseFrom: (candidates collect: [:each | each name]).
	index = 0 ifFalse: [ ^ (candidates at: index) ].