menu messages
browseClassFrom: aClassString
	"Launch a hierarchy browser for the class indicated by the current selection.  If multiple classes matching the selection exist, let the user choose among them."

	| selectedClass |
	"self lineSelectAndEmptyCheck: [ ^ self ]."
	(aClassString isNil or: [ aClassString isEmpty]) ifTrue: [ 
		textArea flash.
		^self ]. 
	
	selectedClass := self model systemNavigation 
		classFromPattern: aClassString 
		withCaption: 'choose a class to browse...'.
	selectedClass isNil
		ifTrue: [ ^ self internalCallToImplementorsOf: aClassString ].
	Smalltalk tools browser openOnClass: selectedClass