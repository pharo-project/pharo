*Tool-Base
browseClassVarRefs: aClass
	"Put up a menu offering all class variable names; if the user chooses one, open up a message-list browser on all methods 
	that refer to the selected class variable"
	| lines labelStream allVars index owningClasses |
	
	"This method should be split into two -- one part that can be tested, and a wrapper that does the UI stuff."
	lines := OrderedCollection new.
	allVars := OrderedCollection new.
	owningClasses := OrderedCollection new.
	labelStream := (String new: 200) writeStream. "Why the heck is a writeStream needed?"
	aClass withAllSuperclasses reverseDo:
		[:class | | vars |
		vars := class classVarNames.
		vars do:
			[:var |
			labelStream nextPutAll: var; cr.
			allVars add: var.
			owningClasses add: class].
		vars isEmpty ifFalse: [lines add: allVars size]].
	labelStream contents isEmpty ifTrue: [^self inform: 'No class variables found']. "handle nil superclass better"
	labelStream skip: -1 "cut last CR".
	index := (UIManager default chooseFrom: (labelStream contents substrings) lines: lines).
	index = 0 ifTrue: [^ self].
	^ self browseAllReferencesTo:
		((owningClasses at: index) classPool associationAt: (allVars at: index))