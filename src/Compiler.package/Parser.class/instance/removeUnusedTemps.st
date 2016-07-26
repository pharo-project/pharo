error correction
removeUnusedTemps
	"Scan for unused temp names, and prompt the user about the prospect of removing each one found"

	| str madeChanges | 
	"I disabled this option. I keep the old code just in case - Hernan Wilkinson"
	self warns ifFalse: [ ^ self ].
	
	madeChanges := false.
	str := requestor text asString.
	((tempsMark between: 1 and: str size)
		and: [(str at: tempsMark) = $|]) ifFalse: [^ self].
	encoder unusedTempNames do:
		[:temp | | start end |
		(UnusedVariable name: temp) ifTrue:
			[(encoder encodeVariable: temp) isUndefTemp
				ifTrue:
					[end := tempsMark.
					["Beginning at right temp marker..."
					start := end - temp size + 1.
					end < temp size or: [temp = (str copyFrom: start to: end)
							and: [(str at: start-1) isSeparator & (str at: end+1) isSeparator]]]
						whileFalse:
							["Search left for the unused temp"
							end := requestor nextTokenFrom: end direction: -1].
					end < temp size ifFalse:
						[(str at: start-1) = $  ifTrue: [start := start-1].
						requestor correctFrom: start to: end with: ''.
						str := str copyReplaceFrom: start to: end with: ''. 
						madeChanges := true.
						tempsMark := tempsMark - (end-start+1)]]
				ifFalse:
					[self inform:
'You''ll first have to remove the\statement where it''s stored into' withCRs]]].
	madeChanges ifTrue: [ReparseAfterSourceEditing signal]