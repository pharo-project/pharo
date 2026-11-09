Extension { #name : 'SycShowLocalSendersCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycShowLocalSendersCommand class >> browserMenuOrder [
	^ 2
]

{ #category : '*Calypso-SystemTools-Core' }
SycShowLocalSendersCommand class >> sourceCodeShortcutActivation [
	<classAnnotation>

	^ ClyShortcutActivation 
		by: $y shift meta
		for: ClyMethodSourceCodeContext
]
