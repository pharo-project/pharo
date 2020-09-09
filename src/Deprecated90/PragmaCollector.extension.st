Extension { #name : #PragmaCollector }

{ #category : #'*Deprecated90' }
PragmaCollector class >> allSystemPragmas [
	self deprecated: 'use Pragma allInstalled instead'.
	^ Pragma allInstalled
]
