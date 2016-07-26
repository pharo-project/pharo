file list menu
fileContentsMenu: aMenu shifted: shifted
	"Construct aMenu to have items appropriate for the file browser's code pane, given the shift state provided"

	| shiftMenu services maybeLine extraLines |
	shifted ifTrue:
		[shiftMenu := SmalltalkEditor shiftedYellowButtonMenu.
		^ aMenu addAllFrom: shiftMenu].
	reference ifNotNil:
		[services := OrderedCollection new.
		(#(briefHex briefFile needToGetBriefHex needToGetBrief) includes: brevityState) ifTrue:
			[services add: self serviceGet].
		(#(fullHex briefHex needToGetFullHex needToGetBriefHex) includes: brevityState) ifFalse:
			[services add: self serviceGetHex].
		(#(needToGetShiftJIS needToGetEUCJP needToGetCNGB needToGetEUCKR needToGetUTF8) includes: brevityState) ifFalse:
			[services add: self serviceGetEncodedText].
		maybeLine := services size.
		(FileStream sourceFileSuffixes includes: self suffixOfSelectedFile) ifTrue:
			[services addAll:
				(self servicesFromSelectorSpecs:
					#(fileIntoNewChangeSet: fileIn: browseChangesFile: browseFile:))].

		extraLines := OrderedCollection new.
		maybeLine > 0 ifTrue: [extraLines add: maybeLine].
		services size > maybeLine ifTrue: [extraLines add: services size].
		aMenu 
			addServices: services
			for: self reference
			extraLines: extraLines].

	aMenu addAllFromPragma: 'fileListContentMenu' target: self.

	^ aMenu
