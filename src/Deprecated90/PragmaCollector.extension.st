Extension { #name : #PragmaCollector }

{ #category : #'*Deprecated90' }
PragmaCollector class >> allSystemPragmas [
	self deprecated: 'use Pragma all instead'.
	^ Pragma all
]
