converting
asText

	| anchor embedMorphSignal |
    anchor := TextAnchor new anchoredMorph: self.
    embedMorphSignal := (Character value: 1) asString. "required by the scanner"
    ^ Text string: embedMorphSignal attribute: anchor.