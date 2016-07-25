private
collectAllSymbolicVersionsFromVersionPragmasInto: symbolicVersionMap using: executionBlock
	| defined versionPragmaDict versionString |
	versionPragmaDict := self extractSymbolicVersionPragmas.
	versionPragmaDict
		keysAndValuesDo: [ :versionSymbol :pragmaColl | 
			defined := false.
			pragmaColl
				do: [ :pragma | 
					defined := true.
					versionString := executionBlock value: versionSymbol value: pragma ].
			defined
				ifTrue: [ 
					versionString == nil
						ifFalse: [ symbolicVersionMap at: versionSymbol put: versionString ]].
			self reset ]