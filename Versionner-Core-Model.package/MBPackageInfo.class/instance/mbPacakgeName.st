as yet unclassified
mbPacakgeName
	| string wc loadedPkgName attributes |
	string := ''.
	(wc := self workingCopy) isNil
		ifTrue: [ ^ string ].
	attributes := OrderedCollection new.
	wc ancestry ancestors notEmpty
		ifTrue: [ 
			loadedPkgName := wc ancestry ancestors first name.
			loadedPkgName = self name
				ifFalse: [ 
					(self compareWorkingCopyNamed: loadedPkgName using: #<=)
						ifTrue: [ attributes add: TextColor red ].
					attributes add: TextEmphasis bold ]].
	self isDirty
		ifTrue: [ attributes add: TextEmphasis underlined ].
	attributes notEmpty 
		ifTrue: [ ^ Text string: string attributes: attributes ].
	^ string