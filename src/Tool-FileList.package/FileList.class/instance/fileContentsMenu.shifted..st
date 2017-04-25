file list menu
fileContentsMenu: aMenu shifted: shifted
	"Construct aMenu to have items appropriate for the file browser's code pane, given the shift state provided"

	| shiftMenu services |
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
		(FileStream sourceFileSuffixes includes: self suffixOfSelectedFile) ifTrue:
			[services addAll: (self itemsForFile: reference)].

		aMenu 
			addServices: services
			for: self reference
			extraLines: #()].

	aMenu addAllFromPragma: 'fileListContentMenu' target: self.

	^ aMenu
