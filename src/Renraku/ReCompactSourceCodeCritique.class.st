"
A code critique to take care of unnecessary separators at end of source code lines and unnecessary dot at end of method
"
Class {
	#name : 'ReCompactSourceCodeCritique',
	#superclass : 'ReAbstractCritique',
	#category : 'Renraku-Critiques',
	#package : 'Renraku',
	#tag : 'Critiques'
}

{ #category : 'instance creation' }
ReCompactSourceCodeCritique class >> for: anEntity selector: aSelector by: aRule [
	^ (self for: anEntity by: aRule)
		tinyHint: aSelector;
		yourself
]

{ #category : 'accessing' }
ReCompactSourceCodeCritique >> change [
	| cm |
	cm := self sourceAnchor sourceEntity.
	^ ReChangeFactory instance
		addMethodSource: (ReMethodSourceCleaner clean: cm sourceCode) 
		in: cm origin
]

{ #category : 'testing' }
ReCompactSourceCodeCritique >> providesChange [

	^ true
]
