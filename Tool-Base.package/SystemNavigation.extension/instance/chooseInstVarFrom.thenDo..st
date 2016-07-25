*Tool-Base
chooseInstVarFrom: aClass thenDo: aBlock 
	"Put up a menu of all the instance variables in the receiver, and when
the user chooses one, evaluate aBlock with the chosen variable as its
parameter.  If the list is 6 or larger, then offer an alphabetical
formulation as an alternative. triggered by a 'show alphabetically' item
at the top of the list."

	| lines labelStream allVars index count offerAlpha |
	(count := aClass allInstVarNames size) = 0 ifTrue: 
		[^ self inform: 'There are no 
instance variables.'].

	allVars := OrderedCollection new.
	lines := OrderedCollection new.
	labelStream := (String new: 200) writeStream.
	(offerAlpha := count > 5)
		ifTrue:
			[lines add: 1.
			allVars add: 'show alphabetically'.
			labelStream nextPutAll: allVars first; cr].
	aClass withAllSuperclasses reverseDo:
		[:class | | vars |
		vars := class instVarNames.
		vars do:
			[:var |
			labelStream nextPutAll: var; cr.
			allVars add: var].
		vars isEmpty ifFalse: [lines add: allVars size]].
	labelStream skip: -1 "cut last CR".
	(lines notEmpty and: [lines last = allVars size]) ifTrue:
		[lines removeLast].  "dispense with inelegant line beneath last item"
	index := (UIManager default chooseFrom: (labelStream contents substrings: {Character cr}) lines: lines
title: 'Instance variables in ', aClass name).
	index = 0 ifTrue: [^ self].
	(index = 1 and: [offerAlpha]) ifTrue: [^ self
chooseInstVarAlphabeticallyFrom: aClass thenDo: aBlock].
	aBlock value: (allVars at: index)