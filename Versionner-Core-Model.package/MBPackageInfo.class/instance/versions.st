as yet unclassified
versions
	""
	|wc v|
	(wc := self workingCopy) isNil
                   ifTrue: [ ^ #() ].
      ^ (wc ancestry breadthFirstAncestors) collect: [ : vi| vi].